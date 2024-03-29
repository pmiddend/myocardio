{-# LANGUAGE OverloadedStrings #-}

module Myocardio.Muscle
  ( Muscle
      ( GluteusMaximus,
        GluteusMedius,
        SerratusAnterior,
        Quadriceps,
        Core,
        SideCore,
        LowerBack,
        UpperBack,
        Balance,
        Calves,
        Neck,
        Deltoid,
        Triceps,
        HipFlexor,
        Pecs,
        Rotators,
        Hamstrings
      ),
    allMuscles,
    muscleToText
  )
where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), Value (String))
import Data.Aeson.Types (prependFailure, typeMismatch)
import Data.Eq (Eq)
import Data.Function (($), (.))
import Data.Ord (Ord)
import Data.Text (unpack)
import Text.Show (Show)
import Prelude
  ( Applicative (pure),
    Bounded (maxBound, minBound),
    Enum (enumFromTo),
    Maybe (Just, Nothing),
    MonadFail (fail),
    Semigroup ((<>)),
  )
import Data.String (IsString)

data Muscle
  = GluteusMaximus
  | GluteusMedius
  | Quadriceps
  | Balance
  | Core
  | SideCore
  | LowerBack
  | UpperBack
  | Calves
  | Neck
  | Deltoid
  | Triceps
  | HipFlexor
  | Pecs
  | SerratusAnterior
  | Rotators
  | Hamstrings
  deriving (Eq, Ord, Show, Enum, Bounded)

allMuscles :: [Muscle]
allMuscles = enumFromTo minBound maxBound

muscleFromText "gluteus maximus" = Just GluteusMaximus
muscleFromText "glute maximus" = Just GluteusMaximus
muscleFromText "glute medius" = Just GluteusMedius
muscleFromText "gluteus medius" = Just GluteusMedius
muscleFromText "quadriceps" = Just Quadriceps
muscleFromText "serratus anterior" = Just SerratusAnterior
muscleFromText "back quadriceps" = Just Quadriceps
muscleFromText "balance" = Just Balance
muscleFromText "core" = Just Core
muscleFromText "side core" = Just SideCore
muscleFromText "lower back" = Just LowerBack
muscleFromText "upper back" = Just UpperBack
muscleFromText "calves" = Just Calves
muscleFromText "neck" = Just Neck
muscleFromText "deltoid" = Just Deltoid
muscleFromText "delta" = Just Deltoid
muscleFromText "triceps" = Just Triceps
muscleFromText "hip flexor" = Just HipFlexor
muscleFromText "pecs" = Just Pecs
muscleFromText "pectoralis" = Just Pecs
muscleFromText "rotators" = Just Rotators
muscleFromText "hamstrings" = Just Hamstrings
muscleFromText _ = Nothing

muscleToText :: IsString p => Muscle -> p
muscleToText GluteusMaximus = "gluteus maximus"
muscleToText Balance = "balance"
muscleToText SerratusAnterior = "serratus anterior"
muscleToText GluteusMedius = "gluteus medius"
muscleToText Quadriceps = "quadriceps"
muscleToText Core = "core"
muscleToText SideCore = "side core"
muscleToText LowerBack = "lower back"
muscleToText UpperBack = "upper back"
muscleToText Calves = "calves"
muscleToText Neck = "neck"
muscleToText Deltoid = "deltoid"
muscleToText Triceps = "triceps"
muscleToText HipFlexor = "hip flexor"
muscleToText Pecs = "pecs"
muscleToText Rotators = "rotators"
muscleToText Hamstrings = "hamstrings"

instance FromJSON Muscle where
  parseJSON (String text) = case muscleFromText text of
    Nothing -> fail $ "unknown muscle \"" <> unpack text <> "\""
    Just success -> pure success
  parseJSON invalid =
    prependFailure
      "parsing muscle failed, "
      (typeMismatch "string" invalid)

instance ToJSON Muscle where
  toJSON = String . muscleToText
