{-# LANGUAGE OverloadedStrings #-}
module Myocardio.FormatTime where

import Myocardio.Util(textShow)
import           Prelude                        ( 
                                                 Integer
                                                , div
                                                )
import           Data.Time.Clock                ( utctDay
                                                , UTCTime
                                                )
import           Data.Text                      ( Text
                                                )
import           Data.Time.Calendar             ( diffDays )
import           Data.Function                  ( on
                                                )
import           Data.Eq                        ( (==) )
import           Data.Ord                       ( (<) )
import           Data.Monoid                    ( (<>) )

formatTimeDiff :: UTCTime -> UTCTime -> Text
formatTimeDiff now utcTime =
  let daydiff :: Integer
      daydiff = (diffDays `on` utctDay) utcTime now
  in  if daydiff == 0
        then "today"
        else if daydiff == 1
          then "yesterday"
          else if daydiff < 7
            then textShow daydiff <> " days ago"
            else textShow (daydiff `div` 7) <> " week(s) ago"
