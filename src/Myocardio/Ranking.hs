{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Myocardio.Ranking (rankExercises, reorderExercises, buildMusclesWithTrainingState, generateComplements, RankedExercise(RankedExercise), rankTime, exerciseComplement) where

import Control.Arrow ((&&&))
import Data.Bifunctor (second)
import Data.Bool (otherwise)
import Data.Composition ((.:))
import Data.Eq (Eq ((/=)), (==))
import Data.Foldable
  ( Foldable (elem),
    maximum,
    minimum,
  )
import Data.Function
  ( const,
    ($),
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
    tail,
    zipWith,
  )
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import Data.Maybe
  ( Maybe (Nothing),
    mapMaybe,
    maybe,
  )
import Data.Monoid ((<>))
import Data.Ord
  ( Down (Down),
    Ord ((<)),
    comparing,
  )
import Data.Time.Clock
  ( UTCTime,
    diffUTCTime,
  )
import Data.Tuple
  ( fst,
    snd,
    uncurry,
  )
import Lens.Micro ((^.))
import Lens.Micro.Platform (makeLensesFor, view)
import Myocardio.Exercise (Exercise, lastL, musclesL, nameL)
import Myocardio.Muscle (Muscle, allMuscles)
import Myocardio.MuscleWithTrainingState (MuscleWithTrainingState (MuscleWithTrainingState))
import Myocardio.TrainingState (TrainingState (Bad, Good, Medium))
import Myocardio.Util (utcTimeDayDiff)
import qualified Myocardio.Util as Util
import Prelude
  ( Double,
    fromIntegral,
    realToFrac,
    (*),
    (+),
    (-),
    (/), Traversable (traverse),
  )

exerciseGroups :: Exercise -> Int
exerciseGroups = length . view musclesL

rankGroups :: Functor f => f Exercise -> f Double
rankGroups = ((fromIntegral . exerciseGroups) <$>)

rankTime :: [Exercise] -> [Double]
rankTime exs =
  let minmax :: Maybe (UTCTime, UTCTime)
      minmax = (minimum &&& maximum) <$> traverse (view lastL) exs
      lerp :: UTCTime -> UTCTime -> UTCTime -> Double
      lerp min max v
        | min == max = 0
        | otherwise =
          100.0
            - realToFrac (diffUTCTime v min * 100 / diffUTCTime max min)
      rank :: UTCTime -> UTCTime -> Maybe UTCTime -> Double
      rank min max = maybe 150 (lerp min max)
   in maybe
        (const 0 <$> exs)
        (\(min, max) -> rank min max <$> (view lastL <$> exs))
        minmax

data RankedExercise = RankedExercise
  { reExercise :: Exercise,
    reRank :: Double
  }

makeLensesFor [("reExercise", "exerciseL"), ("reRank", "rankL")] ''RankedExercise

rankExercises :: [Exercise] -> [Double]
rankExercises exs = zipWith (+) (rankTime exs) (rankGroups exs)

generateComplements :: forall a. [a] -> (a -> NE.NonEmpty a -> Int) -> [a]
generateComplements as complement =
  let rankRec :: [a] -> [a]
      rankRec r = case r of
        [] -> []
        [x] -> [x]
        [x, y] -> [x, y]
        (x : xs') ->
          let xs :: NE.NonEmpty a
              xs = NE.fromList xs'
              complementIndex :: Int
              complementIndex = complement x xs
              rest = withoutIdx complementIndex xs
           in x : (xs NE.!! complementIndex) : rankRec rest
   in rankRec as

withoutIdx :: Int -> NE.NonEmpty a -> [a]
withoutIdx = uncurry (<>) . second tail .: NE.splitAt

exerciseComplement :: RankedExercise -> NE.NonEmpty RankedExercise -> Int
exerciseComplement e rest =
  let em :: [Muscle]
      em = (view musclesL . reExercise) e
      groups :: NE.NonEmpty (NE.NonEmpty (Int, RankedExercise))
      groups =
        NE.groupBy1
          ( \e1 e2 ->
              length (em `intersect` (view musclesL . reExercise . snd) e1)
                == length (em `intersect` (view musclesL . reExercise . snd) e2)
          )
          (NE.zip (NE.fromList [0 ..]) rest)
      sortedGroups :: NE.NonEmpty (NE.NonEmpty (Int, RankedExercise))
      sortedGroups =
        NE.sortBy
          ( comparing
              ( \xs ->
                  length (em `intersect` view musclesL (reExercise (snd (NE.head xs))))
              )
          )
          groups
   in fst
        . NE.head
        . NE.sortBy (comparing (reRank . snd))
        . NE.head
        $ sortedGroups

reorderExercises :: [Exercise] -> [Exercise]
reorderExercises exs =
  let ranked :: [RankedExercise]
      ranked =
        sortOn
          (Down . reRank)
          (zipWith RankedExercise exs (rankExercises exs))
   in reExercise <$> generateComplements ranked exerciseComplement

groupByToMap :: Ord k => (a -> k) -> [a] -> Map.Map k (NE.NonEmpty a)
groupByToMap f = Map.fromList . ((\groupElements -> (f (NE.head groupElements), groupElements)) <$>) . NE.groupWith f

reorderWithChosenElement chosen [] = []
reorderWithChosenElement chosen xs =
  let intersectComparator e = length ((e ^. exerciseL . musclesL) `intersect` (chosen ^. exerciseL . musclesL))
      groupedByIntersection = groupByToMap intersectComparator xs
      nextExercise = NE.head (NE.sortWith (Down . view rankL) (snd (Map.findMin groupedByIntersection)))
      xsWithoutNext = filter ((/= (nextExercise ^. exerciseL . nameL)) . view (exerciseL . nameL)) xs
   in nextExercise : reorderWithChosenElement nextExercise xsWithoutNext

reorderRanked :: [RankedExercise] -> [RankedExercise]
reorderRanked [] = []
reorderRanked [x] = [x]
reorderRanked (x : xs) = x : reorderWithChosenElement x xs

zipWithPrevious g f xs = zipWith f xs (g xs)

reorderPreRanked :: [Exercise] -> [Exercise]
reorderPreRanked = (view exerciseL <$>) . reorderRanked . sortOn (Down . view rankL) . zipWithPrevious rankExercises RankedExercise

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
