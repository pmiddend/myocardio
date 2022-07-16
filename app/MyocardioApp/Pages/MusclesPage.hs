{-# LANGUAGE OverloadedStrings #-}

module MyocardioApp.Pages.MusclesPage (init, view, trainingStateToAttr, cursorLocation, update, Model, attrs) where

import Brick.AttrMap
  ( AttrMap,
    AttrName,
    attrMap,
  )
import Brick.Main
  ( App (App, appAttrMap, appChooseCursor, appDraw, appHandleEvent, appStartEvent),
    continue,
    defaultMain,
    halt,
  )
import Brick.Types
  ( BrickEvent (VtyEvent),
    CursorLocation,
    EventM,
    Next,
    Padding (Pad),
    Widget,
    cursorLocationName,
  )
import Brick.Util
  ( clamp,
    fg,
  )
import Brick.Widgets.Border (vBorder)
import Brick.Widgets.Center (hCenter)
import Brick.Widgets.Core
  ( emptyWidget,
    padRight,
    txt,
    withAttr,
    (<+>),
    (<=>),
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
import Control.Monad (void)
import Control.Monad.IO.Class
  ( MonadIO,
    liftIO,
  )
import Data.Bool (Bool (False, True))
import Data.Eq ((==))
import Data.Foldable (find, foldr)
import Data.Function
  ( const,
    id,
    ($),
    (.),
  )
import Data.Functor ((<$>))
import Data.Int (Int)
import Data.List
  ( length,
    sort,
    sortOn,
  )
import Data.Maybe
  ( Maybe (Just, Nothing),
    isJust,
  )
import Data.Monoid (Monoid (mempty))
import Data.Semigroup (Semigroup ((<>)))
import Data.Text
  ( Text,
    strip,
    unlines,
  )
import qualified Data.Text as Text
import Data.Text.IO (appendFile)
import Data.Text.Zipper
  ( clearZipper,
    getText,
    gotoEOL,
    textZipper,
  )
import Data.Time.Clock (UTCTime, getCurrentTime)
import Graphics.Vty
  ( Attr,
    Event (EvKey),
    Key (KChar, KEnter, KEsc),
    brightGreen,
    brightRed,
    brightYellow,
    cyan,
    defAttr,
    standout,
    underline,
    withStyle,
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
import qualified Lens.Micro.Platform as Lens
import Myocardio.Exercise
  ( Exercise,
    commit,
    last,
    muscles,
    name,
    reps,
    tagged,
    toggleTag,
  )
import Myocardio.ExerciseData (ExerciseData, exercisesL)
import Myocardio.ExerciseId (calculateIds)
import Myocardio.FormatTime (formatTimeDiff)
import Myocardio.Human
  ( FrontOrBack (Back, Front),
    generateHumanMarkup,
  )
import Myocardio.Json
  ( readConfigFile,
    writeConfigFile,
  )
import Myocardio.Muscle (muscleToText)
import Myocardio.MuscleWithTrainingState (MuscleWithTrainingState (MuscleWithTrainingState), trainingState)
import Myocardio.Ranking (buildMusclesWithTrainingState, reorderExercises)
import qualified Myocardio.TablePure as Table
import Myocardio.TrainingState (TrainingState (Bad, Good, Medium))
import MyocardioApp.GlobalData (GlobalData, globalExerciseData, globalNow)
import MyocardioApp.ResourceName (ResourceName (NameEditor, NameList))
import MyocardioApp.UpdateResult (UpdateResult (UpdateResultContinue))
import System.IO (IO)
import Prelude (Show (show), subtract, (+))

trainingStateToAttr :: TrainingState -> AttrName
trainingStateToAttr Good = "muscleGood"
trainingStateToAttr Medium = "muscleMedium"
trainingStateToAttr Bad = "muscleBad"

type Model = GlobalData

stack :: [Widget n] -> Widget n
stack = foldr (<=>) emptyWidget

init :: GlobalData -> Model
init = id

view :: Model -> [Widget ResourceName]
view s =
  let musclesWithTrainingState = buildMusclesWithTrainingState (s ^. globalNow) (s ^. globalExerciseData . exercisesL)
      human = generateHumanMarkup musclesWithTrainingState trainingStateToAttr
      humanFront = human Front
      humanBack = human Back
      divider = vBorder
      muscleMarkedUp :: MuscleWithTrainingState -> Widget ResourceName
      muscleMarkedUp (MuscleWithTrainingState m ts) = withAttr (trainingStateToAttr ts) (txt (muscleToText m))
      muscleWidgets = muscleMarkedUp <$> sortOn (Lens.view trainingState) musclesWithTrainingState
      muscleList = txt "Muscles:" <=> txt "" <=> stack muscleWidgets
   in [humanFront <+> divider <+> humanBack <+> padRight (Pad 1) divider <+> muscleList]

cursorLocation :: Model -> Maybe ResourceName
cursorLocation = const Nothing

update :: Model -> BrickEvent ResourceName e -> EventM ResourceName (UpdateResult Model)
update model _ = pure (UpdateResultContinue model)

attrs :: [(AttrName, Attr)]
attrs =
  [ (trainingStateToAttr Good, fg brightGreen `withStyle` underline),
    (trainingStateToAttr Medium, fg brightYellow `withStyle` standout),
    (trainingStateToAttr Bad, fg brightRed `withStyle` standout)
  ]
