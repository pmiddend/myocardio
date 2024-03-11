{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module MyocardioApp.Database
  ( allMuscles,
    Muscle (..),
    Category (..),
    allCategories,
    Exercise (..),
    ExerciseName (..),
    Intensity (..),
    intensityToText,
    ExerciseWithIntensity (..),
    ExerciseNameWithIntensity (..),
    SorenessValue (..),
    Soreness (..),
    DatabaseF (..),
    emptyDatabase,
    Database,
    exercisesByName,
    readDatabase,
    modifyDb,
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (FromJSON, ToJSON, eitherDecodeFileStrict, encodeFile)
import Data.Bool (not)
import Data.Foldable (Foldable)
import Data.Function (($))
import Data.Functor (Functor, (<$>))
import Data.List (sortBy)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import Data.Maybe (Maybe (Just, Nothing))
import Data.Ord (comparing)
import Data.Text (Text, unpack)
import Data.Time.Clock (UTCTime)
import Data.Traversable (Traversable (traverse))
import GHC.Generics (Generic)
import System.Directory (doesFileExist)
import System.IO (FilePath)
import Prelude (Applicative (pure), Bounded, Either (Left, Right), Enum, Eq, Ord, Read, Show (show), enumFromTo, error, maxBound, minBound)

data Muscle
  = Neck
  | Biceps
  | Triceps
  | Adductors
  | GluteusMedius
  | GluteusMaximus
  | HipFlexors
  | Quadriceps
  | Hamstrings
  | Calves
  deriving (Show, Eq, Generic, Enum, Bounded, Ord, Read)

instance FromJSON Muscle

instance ToJSON Muscle

allMuscles :: [Muscle]
allMuscles = enumFromTo minBound maxBound

data Category
  = Strength
  | Endurance
  | Stretch
  deriving (Show, Eq, Generic, Enum, Bounded, Ord, Read)

instance FromJSON Category

instance ToJSON Category

allCategories :: [Category]
allCategories = enumFromTo minBound maxBound

newtype ExerciseName = ExerciseName {getName :: Text} deriving (Eq, Generic, Ord)

instance Show ExerciseName where
  show (ExerciseName n) = unpack n

instance FromJSON ExerciseName

instance ToJSON ExerciseName

data Exercise = Exercise
  { muscles :: !(NE.NonEmpty Muscle),
    category :: !Category,
    description :: !Text,
    name :: !ExerciseName
  }
  deriving (Show, Eq, Generic)

instance FromJSON Exercise

instance ToJSON Exercise

newtype Intensity = Intensity {getIntensity :: Text} deriving (Show, Eq, Generic)

intensityToText :: Intensity -> Text
intensityToText = getIntensity

instance FromJSON Intensity

instance ToJSON Intensity

data ExerciseWithIntensity a = ExerciseWithIntensity
  { exercise :: !a,
    intensity :: !Intensity,
    time :: !UTCTime
  }
  deriving (Show, Eq, Generic, Functor, Foldable, Traversable)

instance (FromJSON a) => FromJSON (ExerciseWithIntensity a)

instance (ToJSON a) => ToJSON (ExerciseWithIntensity a)

newtype ExerciseNameWithIntensity = ExerciseNameWithIntensity (ExerciseWithIntensity ExerciseName)
  deriving (Show, Eq, Generic)

instance FromJSON ExerciseNameWithIntensity

instance ToJSON ExerciseNameWithIntensity

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

data DatabaseF a = DatabaseF
  { currentTraining :: ![a],
    pastExercises :: ![a],
    sorenessHistory :: ![Soreness],
    exercises :: ![Exercise]
  }
  deriving (Show, Eq, Generic, Functor, Foldable, Traversable)

emptyDatabase :: DatabaseF a
emptyDatabase = DatabaseF {currentTraining = [], pastExercises = [], sorenessHistory = [], exercises = []}

instance (FromJSON a) => FromJSON (DatabaseF a)

instance (ToJSON a) => ToJSON (DatabaseF a)

-- exercises :: [Exercise]
-- exercises =
--   [ Exercise
--       { name = ExerciseName "Running",
--         -- source: https://www.onepeloton.com/blog/what-muscles-does-running-work/
--         muscles = Quadriceps NE.:| [Calves, Hamstrings, HipFlexors, GluteusMaximus, GluteusMedius],
--         category = Endurance,
--         description = L.a_ [L.href_ "https://www.youtube.com/watch?v=wBtOd6_L_3M"] "4 Unexpected Reasons People Hate Running"
--       },
--     Exercise
--       { name = ExerciseName "Lunge",
--         muscles = GluteusMaximus NE.:| [Hamstrings, Quadriceps, Calves, Adductors],
--         category = Strength,
--         description = do
--           L.h2_ "Resources"
--           L.ul_ do
--             L.li_ (L.a_ [L.href_ "https://www.nsca.com/contentassets/24dd7222ed1b4caeb8a0a46b81bd11f3/ptq-4.4.9-the-undervalued-lunge.pdf"] "Has some nice tips for correct execution")
--             L.li_ (L.a_ [L.href_ "https://weighttraining.guide/exercises/lunge/"] "This mentions the muscles involved")
--       }
--   ]

exercisesByName :: DatabaseF a -> Map.Map ExerciseName Exercise
exercisesByName d = Map.fromList ((\e -> (e.name, e)) <$> d.exercises)

newtype DatabaseWithExerciseNames = DatabaseWithExerciseNames (DatabaseF ExerciseNameWithIntensity) deriving (Generic)

instance FromJSON DatabaseWithExerciseNames

instance ToJSON DatabaseWithExerciseNames

dbFile :: FilePath
dbFile = "myocardio.json"

type Database = DatabaseF (ExerciseWithIntensity Exercise)

readDatabase :: (MonadIO m) => m Database
readDatabase = do
  exists <- liftIO (doesFileExist dbFile)
  if not exists
    then pure emptyDatabase
    else do
      result <- liftIO $ eitherDecodeFileStrict dbFile
      case result of
        Left _ -> error "error decoding DB JSON"
        Right (DatabaseWithExerciseNames v) -> do
          let resolveExercise :: ExerciseWithIntensity ExerciseName -> Maybe (ExerciseWithIntensity Exercise)
              resolveExercise = traverse (`Map.lookup` exercisesByName v)
              resolved :: Maybe (DatabaseF (ExerciseWithIntensity Exercise))
              resolved = traverse (\(ExerciseNameWithIntensity e) -> resolveExercise e) v
          case resolved of
            Nothing -> error "invalid exercise name"
            Just resolved' ->
              pure
                ( resolved'
                    { sorenessHistory = sortBy (comparing (.time)) resolved'.sorenessHistory,
                      pastExercises = sortBy (comparing (.time)) resolved'.pastExercises
                    }
                )

writeDatabase :: (MonadIO m) => Database -> m ()
writeDatabase v =
  let encodeDb :: DatabaseF (ExerciseWithIntensity Exercise) -> DatabaseWithExerciseNames
      encodeDb db = DatabaseWithExerciseNames $ (\exWithIn -> ExerciseNameWithIntensity ((.name) <$> exWithIn)) <$> db
   in liftIO $ encodeFile dbFile (encodeDb v)

modifyDb :: (MonadIO m) => (Database -> Database) -> m Database
modifyDb f = do
  db <- readDatabase
  writeDatabase (f db)
  pure (f db)
