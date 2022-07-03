{-# LANGUAGE OverloadedStrings #-}

module Myocardio.FormatTime(formatTimeDiff) where

import Data.Eq ((==))
import Data.Function (on)
import Data.Monoid ((<>))
import Data.Ord ((<))
import Data.Text (Text)
import Data.Time.Calendar (diffDays)
import Data.Time.Clock
  ( UTCTime,
    utctDay,
  )
import Myocardio.Util (textShow)
import Prelude
  ( Integer,
    div,
  )

formatTimeDiff :: UTCTime -> UTCTime -> Text
formatTimeDiff now utcTime =
  let daydiff :: Integer
      daydiff = (diffDays `on` utctDay) now utcTime
   in if daydiff == 0
        then "today"
        else
          if daydiff == 1
            then "yesterday"
            else
              if daydiff < 7
                then textShow daydiff <> " days ago"
                else
                  if daydiff < 14
                    then "a week ago"
                    else textShow (daydiff `div` 7) <> " week(s) ago"
