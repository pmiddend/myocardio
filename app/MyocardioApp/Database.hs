{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module MyocardioApp.Database where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (FromJSON, ToJSON, eitherDecodeFileStrict, encodeFile)
import Data.Bool (not)
import Data.Function (($))
import qualified Data.List.NonEmpty as NE
import Data.Monoid (Monoid (mempty))
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)
import System.Directory (doesFileExist)
import System.IO (FilePath)
import Prelude (Applicative (pure), Bounded, Either (Left, Right), Enum, Eq, Ord, Read, Show, enumFromTo, error, maxBound, minBound)

data Muscle
  = Quadriceps
  | Biceps
  | Triceps
  deriving (Show, Eq, Generic, Enum, Bounded, Ord, Read)

instance FromJSON Muscle

instance ToJSON Muscle

allMuscles :: [Muscle]
allMuscles = enumFromTo minBound maxBound

newtype Exercise = Exercise {getMuscles :: NE.NonEmpty Muscle} deriving (Show, Eq, Generic)

instance FromJSON Exercise

instance ToJSON Exercise

newtype Intensity = Intensity {getIntensity :: Text} deriving (Show, Eq, Generic)

instance FromJSON Intensity

instance ToJSON Intensity

data ExerciseWithIntensity = ExerciseWithIntensity
  { exercise :: !Exercise,
    intensity :: !Intensity,
    time :: !UTCTime
  }
  deriving (Show, Eq, Generic)

instance FromJSON ExerciseWithIntensity

instance ToJSON ExerciseWithIntensity

data SorenessValue
  = VerySore
  | LittleSore
  | NotSore
  deriving (Show, Eq, Generic, Read)

instance FromJSON SorenessValue

instance ToJSON SorenessValue

data Soreness = Soreness
  { time :: !UTCTime,
    muscle :: !Muscle,
    soreness :: !SorenessValue
  }
  deriving (Show, Eq, Generic)

instance FromJSON Soreness

instance ToJSON Soreness

data Database = Database
  { currentTraining :: ![ExerciseWithIntensity],
    pastExercises :: ![ExerciseWithIntensity],
    sorenessHistory :: ![Soreness]
  }
  deriving (Show, Eq, Generic)

emptyDatabase :: Database
emptyDatabase = Database mempty mempty mempty

instance FromJSON Database

instance ToJSON Database

dbFile :: FilePath
dbFile = "myocardio.json"

readDatabase :: (MonadIO m) => m Database
readDatabase = do
  exists <- liftIO (doesFileExist dbFile)
  if not exists
    then pure emptyDatabase
    else do
      result <- liftIO $ eitherDecodeFileStrict dbFile
      case result of
        Left _ -> error "shit"
        Right v -> pure v

writeDatabase :: (MonadIO m) => Database -> m ()
writeDatabase v = liftIO $ encodeFile dbFile v

modifyDb :: (MonadIO m) => (Database -> Database) -> m Database
modifyDb f = do
  db <- readDatabase
  writeDatabase (f db)
  pure (f db)
