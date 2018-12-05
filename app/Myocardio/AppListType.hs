module Myocardio.AppListType where

import           Brick.Widgets.List             ( List)
import           Myocardio.ExerciseId           ( ExerciseId)

type AppListType = List () ExerciseId
