module Myocardio.Util(textShow, maximum, utcTimeDayDiff) where

import Data.Function ((.))
import Data.Text
  ( Text,
    pack,
  )
import Text.Show
  ( Show,
    show,
  )
import Prelude (RealFrac (properFraction), Integral (div))
import Data.Foldable (Foldable (foldMap'))
import Data.Ord (Ord)
import Data.Maybe (Maybe (Just))
import Data.Semigroup (Max(getMax, Max))
import Data.Functor ((<$>))
import Data.Time (UTCTime)
import Data.Int (Int)
import Data.Time.Clock (diffUTCTime)
import Data.Tuple (fst)

textShow :: Show a => a -> Text
textShow = pack . show

maximum :: (Foldable t, Ord a) => t a -> Maybe a
maximum = (getMax <$>) . foldMap' (Just . Max)

utcTimeDayDiff :: UTCTime -> UTCTime -> Int
utcTimeDayDiff now t = fst (properFraction (now `diffUTCTime` t)) `div` 86400
