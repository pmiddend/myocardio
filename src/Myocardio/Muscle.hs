{-# LANGUAGE OverloadedStrings #-}

module Myocardio.Muscle
  ( Muscle
      ( GluteusMaximus,
        GluteusMedius,
        Quadriceps,
        Core,
        SideCore,
        LowerBack,
        UpperBack,
        Calves,
        Neck,
        Delta,
        Triceps,
        HipFlexor,
        Pecs,
        Rotators,
        BackQuadriceps,
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
  | Core
  | SideCore
  | LowerBack
  | UpperBack
  | Calves
  | Neck
  | Delta
  | Triceps
  | HipFlexor
  | Pecs
  | Rotators
  | BackQuadriceps
  | Hamstrings
  deriving (Eq, Ord, Show, Enum, Bounded)

allMuscles :: [Muscle]
allMuscles = enumFromTo minBound maxBound

muscleFromText "gluteus maximus" = Just GluteusMaximus
muscleFromText "gluteus medius" = Just GluteusMedius
muscleFromText "quadriceps" = Just Quadriceps
muscleFromText "core" = Just Core
muscleFromText "side core" = Just SideCore
muscleFromText "lower back" = Just LowerBack
muscleFromText "upper back" = Just UpperBack
muscleFromText "calves" = Just Calves
muscleFromText "neck" = Just Neck
muscleFromText "delta" = Just Delta
muscleFromText "triceps" = Just Triceps
muscleFromText "hip flexor" = Just HipFlexor
muscleFromText "pecs" = Just Pecs
muscleFromText "rotators" = Just Rotators
muscleFromText "back quadriceps" = Just BackQuadriceps
muscleFromText "hamstrings" = Just Hamstrings
muscleFromText _ = Nothing

muscleToText :: IsString p => Muscle -> p
muscleToText GluteusMaximus = "gluteus maximus"
muscleToText GluteusMedius = "gluteus medius"
muscleToText Quadriceps = "quadriceps"
muscleToText Core = "core"
muscleToText SideCore = "side core"
muscleToText LowerBack = "lower back"
muscleToText UpperBack = "upper back"
muscleToText Calves = "calves"
muscleToText Neck = "neck"
muscleToText Delta = "delta"
muscleToText Triceps = "triceps"
muscleToText HipFlexor = "hip flexor"
muscleToText Pecs = "pecs"
muscleToText Rotators = "rotators"
muscleToText BackQuadriceps = "back quadriceps"
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
