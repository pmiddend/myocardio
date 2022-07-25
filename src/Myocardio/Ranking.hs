{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Myocardio.Ranking (rankExercises, reorderExercises, buildMusclesWithTrainingState, RankedExercise (RankedExercise), rankTime) where

import Control.Arrow ((&&&))
import Data.Bool (otherwise)
import Data.Eq (Eq ((/=)), (==))
import Data.Foldable
  ( Foldable (elem),
    maximum,
    minimum,
  )
import Data.Function
  ( const,
    (.),
  )
import Data.Functor
  ( Functor,
    (<$>),
  )
import Data.Int (Int)
import Data.List
  ( filter,
    intersect,
    length,
    sortOn,
    zipWith,
  )
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import Data.Maybe
  ( Maybe (Nothing),
    mapMaybe,
    maybe,
  )
import Data.Ord
  ( Down (Down),
    Ord ((<)),
  )
import Data.Time.Calendar (Day, diffDays)
import Data.Time.Clock
  ( UTCTime (utctDay),
  )
import Data.Tuple
  ( snd,
  )
import Lens.Micro ((^.))
import Lens.Micro.Platform (makeLensesFor, view)
import Myocardio.Exercise (Exercise, last, lastL, musclesL, nameL)
import Myocardio.Muscle (Muscle, allMuscles)
import Myocardio.MuscleWithTrainingState (MuscleWithTrainingState (MuscleWithTrainingState))
import Myocardio.TrainingState (TrainingState (Bad, Good, Medium))
import Myocardio.Util (utcTimeDayDiff)
import qualified Myocardio.Util as Util
import Text.Show (Show)
import Prelude
  ( Double,
    Traversable (traverse),
    fromIntegral,
    (*),
    (+),
    (-),
    (/),
  )

exerciseGroups :: Exercise -> Int
exerciseGroups = length . view musclesL

rankGroups :: Functor f => f Exercise -> f Double
rankGroups = ((fromIntegral . exerciseGroups) <$>)

rankTime :: [Exercise] -> [Double]
rankTime exs =
  let extractLastDay = (utctDay <$>) . last
      minmax :: Maybe (Day, Day)
      minmax = (minimum &&& maximum) <$> traverse extractLastDay exs
      lerp :: Day -> Day -> Day -> Double
      lerp min max v
        | min == max = 0
        | otherwise =
          100.0
            - fromIntegral (diffDays v min * 100) / fromIntegral (diffDays max min)
      rank :: Day -> Day -> Maybe Day -> Double
      rank min max = maybe 150 (lerp min max)
   in maybe
        (const 0 <$> exs)
        (\(min, max) -> rank min max <$> (extractLastDay <$> exs))
        minmax

data RankedExercise = RankedExercise
  { _reExercise :: Exercise,
    _reRank :: Double
  }
  deriving (Show)

makeLensesFor [("_reExercise", "exerciseL"), ("_reRank", "rankL")] ''RankedExercise

rankExercises :: [Exercise] -> [Double]
rankExercises exs = zipWith (+) (rankTime exs) (rankGroups exs)

groupByToMap :: Ord k => (a -> k) -> [a] -> Map.Map k (NE.NonEmpty a)
groupByToMap f = Map.fromList . ((\groupElements -> (f (NE.head groupElements), groupElements)) <$>) . NE.groupAllWith f

reorderWithChosenElement _ [] = []
reorderWithChosenElement chosen xs =
  let intersectComparator :: RankedExercise -> Int
      intersectComparator e = length ((e ^. exerciseL . musclesL) `intersect` (chosen ^. exerciseL . musclesL))
      groupedByIntersection :: Map.Map Int (NE.NonEmpty RankedExercise)
      groupedByIntersection = groupByToMap intersectComparator xs
      minGroup = Map.findMin groupedByIntersection
      nextExercise = NE.head (NE.sortWith (Down . view rankL) (snd minGroup))
      xsWithoutNext = filter ((/= (nextExercise ^. exerciseL . nameL)) . view (exerciseL . nameL)) xs
   in nextExercise : reorderWithChosenElement nextExercise xsWithoutNext

reorderRanked :: [RankedExercise] -> [RankedExercise]
reorderRanked [] = []
reorderRanked [x] = [x]
reorderRanked (x : xs) = x : reorderWithChosenElement x xs

zipWithPrevious g f xs = zipWith f xs (g xs)

reorderPreRanked :: [Exercise] -> [Exercise]
reorderPreRanked = (view exerciseL <$>) . reorderRanked . sortOn (Down . view rankL) . zipWithPrevious rankExercises RankedExercise

reorderExercises :: [Exercise] -> [Exercise]
reorderExercises = reorderPreRanked

trainingStateForMuscle :: UTCTime -> [Exercise] -> Muscle -> TrainingState
trainingStateForMuscle now exs m =
  let lastDates :: [UTCTime]
      lastDates = mapMaybe (\ex -> if m `elem` ex ^. musclesL then ex ^. lastL else Nothing) exs
      evaluateLastDate :: UTCTime -> TrainingState
      evaluateLastDate t =
        let daysSince = now `utcTimeDayDiff` t
         in if daysSince < 5
              then Good
              else
                if daysSince < 10
                  then Medium
                  else Bad
   in maybe Bad evaluateLastDate (Util.maximum lastDates)

buildMusclesWithTrainingState :: UTCTime -> [Exercise] -> [MuscleWithTrainingState]
buildMusclesWithTrainingState now exs =
  let buildForMuscle :: Muscle -> MuscleWithTrainingState
      buildForMuscle m = MuscleWithTrainingState m (trainingStateForMuscle now exs m)
   in buildForMuscle <$> allMuscles
