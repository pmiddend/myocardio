{-# LANGUAGE TemplateHaskell #-}

module MyocardioApp.GlobalData(GlobalData(GlobalData), globalExerciseData, globalNow) where

import Prelude()
import Data.Time.Clock (UTCTime)
import Myocardio.ExerciseData (ExerciseData)
import Lens.Micro.Platform(makeLenses)


data GlobalData = GlobalData
  { _globalExerciseData :: ExerciseData,
    _globalNow :: UTCTime
  }

makeLenses ''GlobalData
