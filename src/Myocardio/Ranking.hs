{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Myocardio.Ranking(rankExercises, reorderExercises) where

import Control.Arrow ((&&&))
import Data.Bifunctor (second)
import Data.Bool (otherwise)
import Data.Composition ((.:))
import Data.Eq ((==))
import Data.Foldable
  ( maximum,
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
  ( intersect,
    length,
    sortOn,
    tail,
    zipWith,
  )
import qualified Data.List.NonEmpty as NE
import Data.Maybe
  ( Maybe,
    mapMaybe,
    maybe,
  )
import Data.Monoid ((<>))
import Data.Ord
  ( Down (Down),
    comparing,
  )
import Data.Text (Text)
import Data.Time.Clock
  ( UTCTime,
    diffUTCTime,
  )
import Data.Tuple
  ( fst,
    snd,
    uncurry,
  )
import Myocardio.Exercise (Exercise (last, muscles))
import Prelude
  ( Double,
    fromIntegral,
    realToFrac,
    (*),
    (+),
    (-),
    (/),
  )

exerciseGroups :: Exercise -> Int
exerciseGroups = length . muscles

rankGroups :: Functor f => f Exercise -> f Double
rankGroups = ((fromIntegral . exerciseGroups) <$>)

rankTime :: [Exercise] -> [Double]
rankTime exs =
  let minmax :: Maybe (UTCTime, UTCTime)
      minmax = (minimum &&& maximum) <$> NE.nonEmpty (mapMaybe last exs)
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
        (\(min, max) -> rank min max <$> (last <$> exs))
        minmax

data RankedExercise = RankedExercise
  { reExercise :: Exercise,
    reRank :: Double
  }

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
  let em :: [Text]
      em = (muscles . reExercise) e
      groups :: NE.NonEmpty (NE.NonEmpty (Int, RankedExercise))
      groups =
        NE.groupBy1
          ( \e1 e2 ->
              length (em `intersect` (muscles . reExercise . snd) e1)
                == length (em `intersect` (muscles . reExercise . snd) e2)
          )
          (NE.zip (NE.fromList [0 ..]) rest)
      sortedGroups :: NE.NonEmpty (NE.NonEmpty (Int, RankedExercise))
      sortedGroups =
        NE.sortBy
          ( comparing
              ( \xs ->
                  length (em `intersect` muscles (reExercise (snd (NE.head xs))))
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
