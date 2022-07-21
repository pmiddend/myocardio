{-# LANGUAGE OverloadedStrings #-}

module MyocardioApp.Pages.MusclesPage (init, view, trainingStateToAttr, cursorLocation, update, Model, attrs) where

import Prelude()
import Brick.AttrMap
  ( AttrName,
  )
import Brick.Types
  ( BrickEvent,
    EventM,
    Padding (Pad),
    Widget,
  )
import Brick.Util
  ( fg,
  )
import Brick.Widgets.Border (vBorder)
import Brick.Widgets.Core
  ( emptyWidget,
    padRight,
    txt,
    withAttr,
    (<+>),
    (<=>),
  )
import Control.Applicative (pure)
import Data.Foldable (foldr)
import Data.Function
  ( const,
    id,
    (.),
  )
import Data.Functor ((<$>))
import Data.List
  ( sortOn,
  )
import Data.Maybe
  ( Maybe (Nothing),
  )
import Graphics.Vty
  ( Attr,
    brightGreen,
    brightRed,
    brightYellow,
    standout,
    underline,
    withStyle,
  )
import Lens.Micro.Platform
  ( (^.),
  )
import qualified Lens.Micro.Platform as Lens
import Myocardio.ExerciseData (exercisesL)
import Myocardio.Human
  ( FrontOrBack (Back, Front),
    generateHumanMarkup,
  )
import Myocardio.Muscle (muscleToText)
import Myocardio.MuscleWithTrainingState (MuscleWithTrainingState (MuscleWithTrainingState), trainingState)
import Myocardio.Ranking (buildMusclesWithTrainingState)
import Myocardio.TrainingState (TrainingState (Bad, Good, Medium))
import MyocardioApp.GlobalData (GlobalData, globalExerciseData, globalNow)
import MyocardioApp.ResourceName (ResourceName)
import MyocardioApp.UpdateResult (UpdateResult (UpdateResultContinue))

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
