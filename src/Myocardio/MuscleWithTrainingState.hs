{-# LANGUAGE TemplateHaskell #-}
module Myocardio.MuscleWithTrainingState(MuscleWithTrainingState(MuscleWithTrainingState), muscleL, trainingStateL) where

import Prelude()
import Myocardio.Muscle (Muscle)
import Myocardio.TrainingState (TrainingState)
import Lens.Micro.Platform (makeLensesFor)

data MuscleWithTrainingState = MuscleWithTrainingState
  { _muscle :: Muscle,
    _trainingState :: TrainingState
  }

makeLensesFor [("_muscle", "muscleL"), ("_trainingState", "trainingStateL")] ''MuscleWithTrainingState
