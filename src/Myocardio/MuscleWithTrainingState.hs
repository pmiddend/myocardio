{-# LANGUAGE TemplateHaskell #-}
module Myocardio.MuscleWithTrainingState(MuscleWithTrainingState(MuscleWithTrainingState), muscle, trainingState) where

import Prelude()
import Myocardio.Muscle (Muscle)
import Myocardio.TrainingState (TrainingState)
import Lens.Micro.Platform (makeLenses)

data MuscleWithTrainingState = MuscleWithTrainingState
  { _muscle :: Muscle,
    _trainingState :: TrainingState
  }

makeLenses ''MuscleWithTrainingState
