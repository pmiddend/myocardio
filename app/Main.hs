{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Brick.AttrMap
  ( AttrMap,
    AttrName,
    attrMap,
  )
import Brick.Main
  ( App (..),
    continue,
    defaultMain,
    halt,
  )
import Brick.Types
  ( BrickEvent (..),
    CursorLocation,
    EventM,
    Next,
    Widget,
    cursorLocationName,
  )
import Brick.Util
  ( clamp,
    fg,
    on,
  )
import Brick.Widgets.Center (hCenter)
import Brick.Widgets.Core
  ( emptyWidget,
    txt,
    (<+>),
    (<=>),
  )
import Brick.Widgets.Edit
  ( applyEdit,
    editContentsL,
    editorText,
    handleEditorEvent,
    renderEditor,
  )
import Control.Applicative (pure)
import Control.Monad (void)
import Control.Monad.IO.Class
  ( MonadIO,
    liftIO,
  )
import Data.Bool (Bool (..))
import Data.Eq ((==))
import Data.Foldable (find, foldMap)
import Data.Function
  ( const,
    ($),
    (.),
  )
import Data.Functor ((<$>))
import Data.Int (Int)
import Data.List
  ( filter,
    length,
    lookup,
    sort,
    zip,
    zipWith,
  )
import Data.Maybe
  ( Maybe (Just, Nothing),
    fromJust,
    isJust,
    listToMaybe,
  )
import Data.Monoid
  ( Monoid (..),
    Sum (..),
    getSum,
    (<>),
  )
import Data.Ord (max)
import Data.Semigroup (Semigroup (..))
import Data.Text
  ( Text,
    justifyLeft,
    strip,
    unlines,
    unwords,
  )
import qualified Data.Text as Text
import Data.Text.IO (appendFile)
import Data.Text.Zipper
  ( clearZipper,
    getText,
    gotoEOL,
    textZipper,
  )
import Data.Time.Clock
  ( UTCTime,
    getCurrentTime,
  )
import qualified Data.Vector as Vec
import Graphics.Vty
  ( Event (..),
    Key (..),
    blue,
    brightGreen,
    brightRed,
    brightYellow,
    cyan,
    defAttr,
    white,
    withStyle,
    bold,
    blink,
    standout,
    underline
  )
import Lens.Micro.Platform
  ( ix,
    to,
    (%~),
    (&),
    (.~),
    (^.),
    (^?!),
  )
import Myocardio.AppListType
import Myocardio.AppState
import Myocardio.Data
  ( Data (..),
    exercisesL,
  )
import Myocardio.Endo (Endo)
import Myocardio.Exercise
  ( Exercise (..),
    commit,
    musclesL,
    repsL,
    toggleTag,
  )
import Myocardio.ExerciseId
  ( ExerciseId,
    calculateIds,
  )
import Myocardio.FormatTime (formatTimeDiff)
import Myocardio.Human (FrontOrBack (..), Muscle (..), MuscleWithTrainingState (..), TrainingState (..), generateHumanMarkup)
import Myocardio.Json
  ( readConfigFile,
    writeConfigFile,
  )
import Myocardio.Ranking (rankExercises, reorderExercises)
import Myocardio.ResourceName
import qualified Myocardio.TablePure as Table
import System.IO (IO, print)
import Prelude (Show (show), subtract, (+))

headings :: [Text]
headings = ["Name", "Reps", "Done?", "Last Execution", "Groups"]

exerciseRows :: AppState -> [[Text]]
exerciseRows s = makeRow <$> reorderExercises (s ^. stateData . exercisesL)
  where
    makeRow ex =
      let lastStr :: Text
          lastStr = case last ex of
            Nothing -> " "
            Just last' -> formatTimeDiff (s ^. stateNow) last'
          taggedStr :: Text
          taggedStr = if isJust (tagged ex) then "*" else " "
       in [name ex, reps ex, taggedStr, lastStr, Text.intercalate "," (sort $ ex ^. musclesL)]

drawUI :: AppState -> [Widget ResourceName]
drawUI state = [humanUi]
  where
    box =
      Table.render
        NameList
        headings
        (exerciseRows state)
        (state ^. stateTableCursor)
        (Table.Alignments mempty mempty Table.AlignLeft Table.AlignTop)
        (Table.Borders False Table.OnlyHeader True)
    footer =
      if state ^. stateEditorFocus
        then ((txt "Reps: ") <+> renderEditor (txt . unlines) (state ^. stateEditorFocus) (state ^. stateEditor))
        else txt "[r]: edit reps [jk]: next/prev [t]: set done [c]: finished [q]: quit"
    ui =
      hCenter box <=> footer
    musclesWithTrainingState =
      [ MuscleWithTrainingState GluteusMaximus Bad,
        MuscleWithTrainingState Neck Good,
        MuscleWithTrainingState Triceps Medium
      ]
    human = generateHumanMarkup musclesWithTrainingState trainingStateToAttr
    humanFront = human Front
    humanBack = human Back
    humanUi = humanFront <+> humanBack

trainingStateToAttr :: TrainingState -> AttrName
trainingStateToAttr Good = "muscleGood"
trainingStateToAttr Medium = "muscleMedium"
trainingStateToAttr Bad = "muscleBad"

theMap :: AttrMap
theMap =
  attrMap
    defAttr
    [ (Table.selectedAttr, fg cyan),
      (trainingStateToAttr Good, fg brightGreen `withStyle` underline),
      (trainingStateToAttr Medium, fg brightYellow  `withStyle` standout),
      (trainingStateToAttr Bad, fg brightRed  `withStyle` standout)
    ]

switchExercises :: MonadIO m => AppState -> [Exercise] -> m AppState
switchExercises state newExs =
  let newExs' = reorderExercises newExs
      ids = calculateIds newExs'
      newState =
        state
          & stateData
            . exercisesL
          .~ newExs'
   in do
        liftIO (writeConfigFile (newState ^. stateData))
        pure newState

withCurrentExercise :: AppState -> (Exercise -> Int -> EventM ResourceName (Next AppState)) -> EventM ResourceName (Next AppState)
withCurrentExercise s f =
  let idx = s ^. stateTableCursor
   in f (s ^?! stateData . exercisesL . ix idx) idx

log :: MonadIO m => Text -> m ()
log logText = do
  now <- liftIO getCurrentTime
  liftIO $ appendFile "/tmp/log.txt" (Text.pack (show now) <> ": " <> logText <> "\n")

handleGlobalEvent s (VtyEvent e) = case e of
  EvKey (KChar 'r') [] ->
    withCurrentExercise s $ \selectedElement _ ->
      continue
        ( s & stateEditorFocus .~ True & stateEditor
            %~ applyEdit
              ( const
                  ( gotoEOL
                      ( textZipper
                          [selectedElement ^. repsL]
                          (Just 1)
                      )
                  )
              )
        )
  EvKey (KChar 'c') [] -> do
    s' <- switchExercises s (commit <$> (s ^. stateData . exercisesL))
    continue s'
  EvKey (KChar 'q') [] -> halt s
  EvKey (KChar 't') [] -> do
    withCurrentExercise s $ \_ idx -> do
      now <- liftIO getCurrentTime
      s' <-
        switchExercises
          s
          ((s ^. stateData . exercisesL) & ix idx %~ toggleTag now)
      continue s'
  _ -> continue s
handleGlobalEvent s _ = continue s

confirmsEditor (VtyEvent (EvKey KEnter [])) = True
confirmsEditor _ = False

terminatesEditor (VtyEvent (EvKey KEsc [])) = True
terminatesEditor _ = False

handleEditorEvent' (VtyEvent e) s = handleEditorEvent e s
handleEditorEvent' _ s = pure s

modifyCursorPosition :: AppState -> (Int -> Int) -> Int -> Int
modifyCursorPosition s f current = clamp 0 (length (s ^. stateData . exercisesL)) (f current)

handleTableEvent :: AppState -> BrickEvent ResourceName e -> EventM ResourceName (Maybe AppState)
handleTableEvent s (VtyEvent (EvKey (KChar 'j') [])) = pure (Just (s & stateTableCursor %~ modifyCursorPosition s (+ 1)))
handleTableEvent s (VtyEvent (EvKey (KChar 'k') [])) = pure (Just (s & stateTableCursor %~ modifyCursorPosition s (subtract 1)))
handleTableEvent _ _ = pure Nothing

appEvent ::
  AppState ->
  BrickEvent ResourceName e ->
  EventM ResourceName (Next AppState)
appEvent s e =
  if s ^. stateEditorFocus
    then
      if confirmsEditor e
        then withCurrentExercise s $ \_ idx -> do
          let editContent :: Text
              editContent = s ^. stateEditor . editContentsL . to (strip . unlines . getText)
              newExs :: [Exercise]
              newExs = (s ^. stateData . exercisesL) & ix idx . repsL .~ editContent
          s' <- switchExercises s newExs
          continue (s' & stateEditorFocus .~ False & stateEditor %~ applyEdit clearZipper)
        else
          if terminatesEditor e
            then continue (s & stateEditorFocus .~ False & stateEditor %~ applyEdit clearZipper)
            else do
              newEditor <- handleEditorEvent' e (s ^. stateEditor)
              continue (s & stateEditor .~ newEditor)
    else do
      newTable <- handleTableEvent s e
      case newTable of
        Just newTable' -> continue newTable'
        Nothing -> handleGlobalEvent s e

appCursor ::
  AppState ->
  [CursorLocation ResourceName] ->
  Maybe (CursorLocation ResourceName)
appCursor s cl =
  let filterList x = find ((== Just x) . cursorLocationName) cl
   in filterList (if s ^. stateEditorFocus then NameEditor else NameList)

main :: IO ()
main = do
  configFile <- readConfigFile
  now <- getCurrentTime
  let app =
        App
          { appDraw = drawUI,
            appChooseCursor = appCursor,
            appHandleEvent = appEvent,
            appStartEvent = pure,
            appAttrMap = const theMap
          }
      initialState =
        AppState
          0
          (editorText NameEditor (Just 1) "")
          configFile
          now
          False
  void $ defaultMain app initialState
