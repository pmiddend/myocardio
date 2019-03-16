{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Myocardio.Ranking where

import           Data.Ord                       ( comparing
                                                , Down(..)
                                                )
import           Data.Text                      ( Text )
import           Data.Composition               ( (.:) )
import           Data.Tuple                     ( uncurry
                                                , fst
                                                , snd
                                                )
import           Data.Monoid                    ( (<>) )
import           Data.Bifunctor                 ( second )
import           Data.Int                       ( Int )
import           Data.Bool                      ( otherwise
                                                , Bool
                                                )
import           Data.Eq                        ( (==) )
import           Data.Maybe                     ( Maybe(..)
                                                , mapMaybe
                                                , listToMaybe
                                                , maybe
                                                )
import           Myocardio.Exercise             ( Exercise(last, muscles) )
import           Prelude                        ( realToFrac
                                                , Double
                                                , (/)
                                                , (-)
                                                , (*)
                                                , (+)
                                                , fromIntegral
                                                )
import           Data.Time.Clock                ( UTCTime
                                                , diffUTCTime
                                                )
import           Data.Functor                   ( (<$>)
                                                , Functor
                                                )
import           Data.Function                  ( (.)
                                                , const
                                                , ($)
                                                )
import qualified Data.List.NonEmpty            as NE
import           Data.List                      ( zipWith
                                                , sortOn
                                                , tail
                                                , intersect
                                                , length
                                                )
import           Data.Foldable                  ( minimum
                                                , maximum
                                                )
import           Control.Arrow                  ( (&&&) )

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
        | otherwise = 100.0
        - realToFrac (diffUTCTime v min * 100 / diffUTCTime max min)
      rank :: UTCTime -> UTCTime -> Maybe UTCTime -> Double
      rank min max = maybe 150 (lerp min max)
  in  maybe (const 0 <$> exs)
            (\(min, max) -> rank min max <$> (last <$> exs))
            minmax

data RankedExercise = RankedExercise {
    reExercise :: Exercise
  , reRank :: Double
  }

rankExercises :: [Exercise] -> [Double]
rankExercises exs = zipWith (+) (rankTime exs) (rankGroups exs)

generateComplements :: forall a . [a] -> (a -> NE.NonEmpty a -> Int) -> [a]
generateComplements as complement =
  let rankRec :: [a] -> [a]
      rankRec r = case r of
        []     -> []
        [x]    -> [x]
        [x, y] -> [x, y]
        (x : xs') ->
          let xs :: NE.NonEmpty a
              xs = NE.fromList xs'
              complementIndex :: Int
              complementIndex = complement x xs
              rest            = withoutIdx complementIndex xs
          in  x : (xs NE.!! complementIndex) : rankRec rest
  in  rankRec as

withoutIdx :: Int -> NE.NonEmpty a -> [a]
withoutIdx = uncurry (<>) . second tail .: NE.splitAt

exerciseComplement :: RankedExercise -> NE.NonEmpty RankedExercise -> Int
exerciseComplement e rest
  = let
      em :: [Text]
      em = (muscles . reExercise) e
      groups :: NE.NonEmpty (NE.NonEmpty (Int, RankedExercise))
      groups = NE.groupBy1
        (\e1 e2 -> length (em `intersect` (muscles . reExercise . snd) e1)
          == length (em `intersect` (muscles . reExercise . snd) e2)
        )
        (NE.zip (NE.fromList [0 ..]) rest)
      sortedGroups :: NE.NonEmpty (NE.NonEmpty (Int, RankedExercise))
      sortedGroups = NE.sortBy
        (comparing
          (\xs ->
            length (em `intersect` muscles (reExercise (snd (NE.head xs))))
          )
        )
        groups
    in
      fst
      . NE.head
      . NE.sortBy (comparing (reRank . snd))
      . NE.head
      $ sortedGroups

reorderExercises :: [Exercise] -> [Exercise]
reorderExercises exs =
  let ranked :: [RankedExercise]
      ranked = sortOn (Down . reRank)
                      (zipWith RankedExercise exs (rankExercises exs))
  in  reExercise <$> generateComplements ranked exerciseComplement

indexOfFirst :: (a -> Bool) -> NE.NonEmpty a -> Maybe Int
indexOfFirst p =
  (fst <$>) . listToMaybe . NE.filter (p . snd) . NE.zip (NE.fromList [0 ..])
