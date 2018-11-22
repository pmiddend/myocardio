{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Myocardio.Ranking where

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
import           Data.Eq                        ( (==)
                                                , (/=)
                                                )
import           Data.Maybe                     ( Maybe(..)
                                                , fromMaybe
                                                , mapMaybe
                                                , listToMaybe
                                                , maybe
                                                )
import           Myocardio.Exercise             ( Exercise(dates, muscles) )
import           Prelude                        ( realToFrac
                                                , mod
                                                , undefined
                                                , Double
                                                , (/)
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
                                                )
import qualified Data.List.NonEmpty            as NE
import           Data.List                      ( zipWith
                                                , tail
                                                , length
                                                )
import           Data.Foldable                  ( minimum
                                                , maximum
                                                )
import           Control.Arrow                  ( (&&&) )

exerciseLastDate :: Exercise -> Maybe UTCTime
exerciseLastDate = listToMaybe . dates

exerciseGroups :: Exercise -> Int
exerciseGroups = length . muscles

rankGroups :: Functor f => f Exercise -> f Double
rankGroups = ((fromIntegral . exerciseGroups) <$>)

rankTime :: [Exercise] -> [Double]
rankTime exs =
  let minmax :: Maybe (UTCTime, UTCTime)
      minmax =
        (minimum &&& maximum) <$> NE.nonEmpty (mapMaybe exerciseLastDate exs)
      lerp :: UTCTime -> UTCTime -> UTCTime -> Double
      lerp min max v
        | min == max = 0
        | otherwise = realToFrac (diffUTCTime v min * 100 / diffUTCTime max min)
      rank :: UTCTime -> UTCTime -> Maybe UTCTime -> Double
      rank min max = maybe 0 (lerp min max)
  in  maybe (const 0 <$> exs)
            (\(min, max) -> rank min max <$> (exerciseLastDate <$> exs))
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
        []  -> []
        [x] -> [x]
        [x,y] -> [x,y]
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

reorderExercises :: [Exercise] -> [Exercise]
reorderExercises exs =
  let ranked :: [RankedExercise]
      ranked = zipWith RankedExercise exs (rankExercises exs)
      complement :: RankedExercise -> NE.NonEmpty RankedExercise -> Int
      complement = undefined
  in  reExercise <$> generateComplements ranked complement

indexOfFirst :: (a -> Bool) -> NE.NonEmpty a -> Maybe Int
indexOfFirst p =
  (fst <$>) . listToMaybe . NE.filter (p . snd) . NE.zip (NE.fromList [0 ..])

testComplement :: [Int]
testComplement =
  let f x ys
        | x `mod` 2 == 0 = fromMaybe 0 (indexOfFirst ((/= 0) . (`mod` 2)) ys)
        | otherwise      = fromMaybe 0 (indexOfFirst ((== 0) . (`mod` 2)) ys)
  in  generateComplements ([1, 3, 5, 7, 2, 4, 6, 6] :: [Int]) f
