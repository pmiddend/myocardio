{-# LANGUAGE NoImplicitPrelude #-}
module Myocardio.Ranking where

import           Data.Bool                      ( otherwise )
import           Data.Eq                        ( (==) )
import           Data.Maybe                     ( Maybe
                                                , listToMaybe
                                                , maybe
                                                , mapMaybe
                                                )
import           Myocardio.Exercise             ( Exercise(dates) )
import           Prelude                        ( realToFrac
                                                , Double
                                                , (/)
                                                , (*)
                                                )
import           Data.Time.Clock                ( UTCTime
                                                , diffUTCTime
                                                )
import           Data.Functor                   ( (<$>) )
import           Data.Function                  ( (.)
                                                , const
                                                )
import           Data.List.NonEmpty             ( nonEmpty )
import           Data.Foldable                  ( minimum
                                                , maximum
                                                )
import           Control.Arrow                  ( (&&&) )

exerciseLastDate :: Exercise -> Maybe UTCTime
exerciseLastDate = listToMaybe . dates

rankTime :: [Exercise] -> [Double]
rankTime exs =
  let minmax :: Maybe (UTCTime, UTCTime)
      minmax =
        (minimum &&& maximum) <$> nonEmpty (mapMaybe exerciseLastDate exs)
      lerp :: UTCTime -> UTCTime -> UTCTime -> Double
      lerp min max v
        | min == max = 0
        | otherwise = realToFrac (diffUTCTime v min * 100 / diffUTCTime max min)
      rank :: UTCTime -> UTCTime -> Maybe UTCTime -> Double
      rank min max = maybe 0 (lerp min max)
  in  maybe (const 0 <$> exs)
            (\(min, max) -> rank min max <$> (exerciseLastDate <$> exs))
            minmax
