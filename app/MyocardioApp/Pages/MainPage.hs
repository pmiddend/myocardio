{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module MyocardioApp.Pages.MainPage (Model, view, update, cursorLocation, init, attrs, deinit) where

import Brick.AttrMap
  ( AttrName,
  )
import Brick.Types
  ( BrickEvent (VtyEvent),
    EventM,
    Widget, Padding (Max),
  )
import Brick.Util
  ( clamp,
    bg
  )
import Brick.Widgets.Center (hCenter)
import Brick.Widgets.Core
  ( txt,
    (<+>),
    (<=>), withAttr, padRight,
  )
import Brick.Widgets.Edit
  ( Editor,
    applyEdit,
    editContentsL,
    editorText,
    handleEditorEvent,
    renderEditor,
  )
import Control.Applicative (pure)
import Control.Monad.IO.Class
  ( MonadIO,
    liftIO,
  )
import Data.Bool (Bool (False, True), otherwise)
import Data.Function
  ( const,
    ($),
    (.),
  )
import Data.Functor ((<$>))
import Data.Int (Int)
import Data.List
  ( length,
    sort,
  )
import Data.Maybe
  ( Maybe (Just, Nothing),
    isJust,
  )
import Data.Monoid (Monoid (mempty))
import Data.Text
  ( Text,
    strip,
    unlines,
  )
import qualified Data.Text as Text
import Data.Text.Zipper
  ( clearZipper,
    getText,
    gotoEOL,
    textZipper,
  )
import Data.Time.Clock (UTCTime, getCurrentTime)
import Graphics.Vty
  ( Event (EvKey),
    Key (KChar, KEnter, KEsc),
    Attr,
    withStyle,
    bold,
    brightGreen,
  )
import Lens.Micro.Platform
  ( ix,
    makeLenses,
    to,
    (%~),
    (&),
    (.~),
    (^.),
    (^?!),
  )
import Myocardio.Exercise
  ( Exercise,
    commit,
    lastL,
    musclesL,
    nameL,
    repsL,
    taggedL,
    toggleTag,
  )
import Myocardio.ExerciseData (ExerciseData, exercisesL)
import Myocardio.FormatTime (formatTimeDiff)
import MyocardioApp.ConfigJson
  ( writeConfigFile,
  )
import Myocardio.Muscle (muscleToText)
import Myocardio.Ranking (reorderExercises)
import qualified MyocardioApp.TablePure as Table
import MyocardioApp.GlobalData (GlobalData(GlobalData), globalExerciseData, globalNow)
import MyocardioApp.ResourceName (ResourceName (NameEditor, NameList))
import MyocardioApp.UpdateResult (UpdateResult (UpdateResultContinue, UpdateResultHalt))
import Prelude (subtract, (+), (-))

data Model = Model
  { _editorFocus :: Bool,
    _editor :: Editor Text ResourceName,
    _tableCursor :: Int,
    _exerciseData :: ExerciseData,
    _now :: UTCTime
  }

makeLenses ''Model

deinit :: Model -> GlobalData
deinit model = GlobalData (model ^. exerciseData) (model ^. now)

init :: GlobalData -> Model
init globalData =
  Model
    { _editorFocus = False,
      _editor = editorText NameEditor (Just 1) "",
      _tableCursor = 0,
      _exerciseData = (globalData ^. globalExerciseData) & exercisesL %~ reorderExercises,
      _now = globalData ^. globalNow
    }

headings :: [Text]
headings = ["Name", "Reps", "Done?", "Last Execution", "Groups"]

exerciseRows :: Model -> [[Text]]
exerciseRows model = makeRow <$> (model ^. exerciseData . exercisesL)
  where
    makeRow :: Exercise -> [Text]
    makeRow ex =
      let lastStr :: Text
          lastStr = case ex ^. lastL of
            Nothing -> " "
            Just last' -> formatTimeDiff (model ^. now) last'
          taggedStr :: Text
          taggedStr = if isJust (ex ^. taggedL) then "*" else " "
       in [ex ^. nameL, ex ^. repsL, taggedStr, lastStr, Text.intercalate "," (muscleToText <$> sort (ex ^. musclesL))]

statusLineAttr :: AttrName
statusLineAttr = "status line"

attrs :: [(AttrName, Attr)]
attrs =
  [ (statusLineAttr, bg brightGreen `withStyle` bold) ]

view :: Model -> Widget ResourceName
view model = hCenter box <=> footer
  where
    box =
      Table.render
        NameList
        headings
        (exerciseRows model)
        (model ^. tableCursor)
        (Table.Alignments mempty mempty Table.AlignLeft Table.AlignTop)
        (Table.Borders False Table.OnlyHeader True)
    footer =
      if model ^. editorFocus
        then txt "Reps: " <+> renderEditor (txt . unlines) (model ^. editorFocus) (model ^. editor)
        else withAttr statusLineAttr $ padRight Max $ txt "[r]: edit reps [jk]: next/prev [t]: set done [c]: finished [q]: quit"

switchExercises :: MonadIO m => Model -> [Exercise] -> m Model
switchExercises model newExs =
  let newExs' = reorderExercises newExs
      newModel =
        model
          & exerciseData
            . exercisesL
          .~ newExs'
   in do
        liftIO (writeConfigFile (newModel ^. exerciseData))
        pure newModel

withCurrentExercise :: Model -> (Exercise -> Int -> EventM ResourceName (UpdateResult Model)) -> EventM ResourceName (UpdateResult Model)
withCurrentExercise model f =
  let idx = model ^. tableCursor
   in f (model ^?! exerciseData . exercisesL . ix idx) idx

updateTable :: Model -> BrickEvent ResourceName e -> EventM ResourceName (Maybe Model)
updateTable model (VtyEvent (EvKey (KChar 'j') [])) = pure (Just (model & tableCursor %~ modifyCursorPosition model (+ 1)))
updateTable model (VtyEvent (EvKey (KChar 'k') [])) = pure (Just (model & tableCursor %~ modifyCursorPosition model (subtract 1)))
updateTable _ _ = pure Nothing


update :: Model -> BrickEvent ResourceName e -> EventM ResourceName (UpdateResult Model)
update model e | model ^. editorFocus = updateEditorFocused model e
               | otherwise = do
                   newTable <- updateTable model e
                   case newTable of
                     Just newTable' -> pure $ UpdateResultContinue newTable'
                     Nothing -> updateGlobal model e

confirmsEditor (VtyEvent (EvKey KEnter [])) = True
confirmsEditor _ = False

terminatesEditor (VtyEvent (EvKey KEsc [])) = True
terminatesEditor _ = False

handleEditorEvent' (VtyEvent e) s = handleEditorEvent e s
handleEditorEvent' _ s = pure s

modifyCursorPosition :: Model -> (Int -> Int) -> Int -> Int
modifyCursorPosition model f current = clamp 0 (length (model ^. exerciseData . exercisesL) - 1) (f current)

updateEditorFocused :: Model -> BrickEvent ResourceName e -> EventM ResourceName (UpdateResult Model)
updateEditorFocused model e | confirmsEditor e = do
                                withCurrentExercise model $ \_ idx -> do
                                  let editContent :: Text
                                      editContent = model ^. editor . editContentsL . to (strip . unlines . getText)
                                      newExs :: [Exercise]
                                      newExs = (model ^. exerciseData . exercisesL) & ix idx . repsL .~ editContent
                                  model' <- switchExercises model newExs
                                  pure $ UpdateResultContinue (model' & editorFocus .~ False & editor %~ applyEdit clearZipper)
                            | terminatesEditor e = pure $ UpdateResultContinue (model & editorFocus .~ False & editor %~ applyEdit clearZipper)
                            | otherwise = do
                                newEditor <- handleEditorEvent' e (model ^. editor)
                                pure $ UpdateResultContinue $ model & editor .~ newEditor

-- log :: MonadIO m => Text -> m ()
-- log logText = do
--   now' <- liftIO getCurrentTime
--   liftIO $ appendFile "/tmp/log.txt" (Text.pack (show now') <> ": " <> logText <> "\n")


updateGlobal :: Model -> BrickEvent ResourceName e -> EventM ResourceName (UpdateResult Model)
updateGlobal model e = case e of
  VtyEvent (EvKey (KChar 'r') []) ->
    withCurrentExercise model $ \selectedElement _ ->
      pure $ UpdateResultContinue $
        model & editorFocus .~ True & editor
            %~ applyEdit
              ( const
                  ( gotoEOL
                      ( textZipper
                          [selectedElement ^. repsL]
                          (Just 1)
                      )
                  )
              )
  VtyEvent (EvKey (KChar 'q') []) -> pure $ UpdateResultHalt model
  VtyEvent (EvKey (KChar 'c') []) -> do
    model' <- switchExercises model (commit <$> (model ^. exerciseData . exercisesL))
    pure $ UpdateResultContinue model'
  VtyEvent (EvKey (KChar 't') []) -> do
    withCurrentExercise model $ \_ idx -> do
      now' <- liftIO getCurrentTime
      model' <-
        switchExercises
          model
          ((model ^. exerciseData . exercisesL) & ix idx %~ toggleTag now')
      pure $ UpdateResultContinue model'
  _ -> pure $ UpdateResultContinue model

cursorLocation :: Model -> Maybe ResourceName
cursorLocation m = Just $ if m ^. editorFocus then NameEditor else NameList
