module Myocardio.AppListType where

import           Brick.Widgets.List             ( List)
import           Myocardio.ExerciseId           ( ExerciseId)
import           Myocardio.ResourceName           ( ResourceName)

type AppListType = List ResourceName ExerciseId
