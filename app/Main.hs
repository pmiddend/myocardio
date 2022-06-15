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
  ( fg,
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
import Brick.Widgets.List
  ( list,
    listAttr,
    listElementsL,
    listMoveDown,
    listMoveUp,
    listSelectedAttr,
    listSelectedL,
    renderList,
  )
import Control.Applicative (pure)
import Control.Monad (void)
import Control.Monad.IO.Class
  ( MonadIO,
    liftIO,
  )
import Data.Bool (Bool (..))
import Data.Eq ((==))
import Data.Foldable (foldMap)
import Data.Function
  ( const,
    ($),
    (.),
  )
import Data.Functor ((<$>))
import Data.Int (Int)
import Data.List
  ( filter,
    lookup,
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
    length,
    strip,
    unlines,
    unwords,
  )
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
    cyan,
    defAttr,
    white,
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
    repsL,
    toggleTag,
  )
import Myocardio.ExerciseId
  ( ExerciseId,
    calculateIds,
  )
import Myocardio.FormatTime (formatTimeDiff)
import Myocardio.Json
  ( readConfigFile,
    writeConfigFile,
  )
import Myocardio.Ranking (rankExercises, reorderExercises)
import Myocardio.ResourceName
import System.IO (IO, print)
import Prelude ((+))

newtype ColumnSizes = ColumnSizes
  { getColSizes :: [Int]
  }

colSizeApply :: (Int -> Int -> Int) -> ColumnSizes -> ColumnSizes -> ColumnSizes
colSizeApply f (ColumnSizes a) (ColumnSizes b) = ColumnSizes (zipWith f a b)

instance Semigroup ColumnSizes where
  (<>) = colSizeApply max

instance Monoid ColumnSizes where
  mempty = ColumnSizes [0 ..]

applySizes :: ColumnSizes -> [Text] -> Text
applySizes sizes cols =
  let just :: Int -> Text -> Text
      just i = justifyLeft (i + 1) ' '
   in unwords (zipWith just (getColSizes sizes) cols)

listDrawElement ::
  UTCTime ->
  ColumnSizes ->
  [(ExerciseId, Exercise)] ->
  Bool ->
  ExerciseId ->
  Widget ResourceName
listDrawElement now sizes exs _ exId =
  let ex :: Exercise
      ex = fromJust (lookup exId exs)
      lastStr :: Text
      lastStr = foldMap (formatTimeDiff now) (last ex)
      taggedStr :: Text
      taggedStr = if isJust (tagged ex) then "*" else " "
      row = [name ex, reps ex, taggedStr, lastStr]
   in txt (applySizes sizes row)

headings :: [Text]
headings = ["Name", "Reps", "T", "Last"]

headingSizes :: ColumnSizes
headingSizes = ColumnSizes (length <$> headings)

colSizes :: UTCTime -> [Exercise] -> ColumnSizes
colSizes _ [] = mempty
colSizes now exs =
  let sizeSingle :: Exercise -> ColumnSizes
      sizeSingle ex =
        headingSizes
          <> ColumnSizes
            [ length (name ex),
              length (reps ex),
              1,
              getSum . foldMap (Sum . length . formatTimeDiff now) . last $ ex
            ]
   in foldMap sizeSingle exs

drawUI :: AppState -> [Widget ResourceName]
drawUI state = [ui]
  where
    exs = state ^. stateData . exercisesL
    colSizes' = colSizes (state ^. stateNow) (state ^. stateData . exercisesL)
    ids = calculateIds exs
    box =
      renderList
        (listDrawElement (state ^. stateNow) colSizes' (zip ids exs))
        True
        (state ^. stateList)
    editorHeading =
      if state ^. stateEditorFocus then txt "Reps: " else emptyWidget
    ui =
      txt (applySizes colSizes' headings)
        <=> hCenter box
        <=> ( editorHeading
                <+> renderEditor
                  (txt . unlines)
                  (state ^. stateEditorFocus)
                  (state ^. stateEditor)
            )

customAttr :: AttrName
customAttr = listSelectedAttr <> "custom"

theMap :: AttrMap
theMap =
  attrMap
    defAttr
    [ (listAttr, white `on` blue),
      (listSelectedAttr, blue `on` white),
      (customAttr, fg cyan)
    ]

modifyList ::
  Endo AppListType -> AppState -> EventM ResourceName (Next AppState)
modifyList f state = continue (state & stateList %~ f)

switchExercises :: MonadIO m => AppState -> [Exercise] -> m AppState
switchExercises state newExs =
  let newExs' = reorderExercises newExs
      ids = calculateIds newExs'
      newState =
        state
          & stateData
            . exercisesL
          .~ newExs'
          & stateList
            . listElementsL
          .~ Vec.fromList ids
   in do
        liftIO (writeConfigFile (newState ^. stateData))
        pure newState

maybeEditor ::
  AppState ->
  Event ->
  EventM ResourceName (Next AppState) ->
  EventM ResourceName (Next AppState)
maybeEditor s e f =
  if s ^. stateEditorFocus
    then do
      newEditor <- handleEditorEvent e (s ^. stateEditor)
      continue (s & stateEditor .~ newEditor)
    else f

withCurrentElement :: AppState -> (Exercise -> Int -> EventM ResourceName (Next AppState)) -> EventM ResourceName (Next AppState)
withCurrentElement s f =
  case s ^. stateList . listSelectedL of
    Nothing -> continue s
    Just idx -> f (s ^?! stateData . exercisesL . ix idx) idx

appEvent ::
  AppState ->
  BrickEvent ResourceName e ->
  EventM ResourceName (Next AppState)
appEvent s (VtyEvent e) = case e of
  EvKey (KChar 'j') [] -> maybeEditor s e (modifyList listMoveDown s)
  EvKey (KChar 'k') [] -> maybeEditor s e (modifyList listMoveUp s)
  EvKey (KChar 'q') [] -> maybeEditor s e (halt s)
  EvKey (KChar 'r') [] ->
    maybeEditor s e $
      withCurrentElement s $ \selectedElement _ ->
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
  EvKey (KChar 'c') [] -> maybeEditor s e $ do
    s' <- switchExercises s (commit <$> (s ^. stateData . exercisesL))
    continue s'
  EvKey (KChar 't') [] ->
    maybeEditor s e $
      withCurrentElement s $ \_ idx -> do
        now <- liftIO getCurrentTime
        s' <-
          switchExercises
            s
            ((s ^. stateData . exercisesL) & ix idx %~ toggleTag now)
        continue s'
  EvKey KEnter [] ->
    if s ^. stateEditorFocus
      then withCurrentElement s $ \_ idx -> do
        let editContent :: Text
            editContent = s ^. stateEditor . editContentsL . to (strip . unlines . getText)
            newExs :: [Exercise]
            newExs = (s ^. stateData . exercisesL) & ix idx . repsL .~ editContent
        s' <- switchExercises s newExs
        continue (s' & stateEditorFocus .~ False & stateEditor %~ applyEdit clearZipper)
      else continue s
  EvKey KEsc [] ->
    if s ^. stateEditorFocus
      then
        continue
          (s & stateEditorFocus .~ False & stateEditor %~ applyEdit clearZipper)
      else halt s
  _ -> maybeEditor s e (continue s)
appEvent s _ = continue s

appCursor ::
  AppState ->
  [CursorLocation ResourceName] ->
  Maybe (CursorLocation ResourceName)
appCursor s cl =
  let filterList x = listToMaybe (filter ((== Just x) . cursorLocationName) cl)
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
      exs = reorderExercises (exercises configFile)
      ids = calculateIds exs
      initialState =
        AppState
          (list NameList (Vec.fromList ids) 1)
          (editorText NameEditor (Just 1) "")
          configFile
          now
          False
  void $ defaultMain app initialState
