{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Myocardio.ExerciseData(ExerciseData(..), categoriesL, exercisesL, emptyExerciseData) where

import Data.Aeson
  ( FromJSON,
    ToJSON,
  )
import Data.Text (Text)
import GHC.Generics (Generic)
import Lens.Micro.Platform (makeLensesFor)
import Myocardio.Exercise (Exercise)
import Text.Show (Show)

data ExerciseData = ExerciseData
  { categories :: [Text],
    exercises :: [Exercise]
  }
  deriving (Generic, Show, ToJSON, FromJSON)

makeLensesFor [("categories", "categoriesL"), ("exercises", "exercisesL")] ''ExerciseData

emptyExerciseData :: ExerciseData
emptyExerciseData = ExerciseData [] []
