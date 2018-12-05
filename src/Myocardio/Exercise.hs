{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Myocardio.Exercise where

import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           Data.Maybe                     ( Maybe(Nothing,Just), isJust )
import           Data.Text                      ( Text )
import           Data.Time.Clock                ( UTCTime )
import           GHC.Generics                   ( Generic )
import           Text.Show                      ( Show )
import           Data.Eq                        ( Eq )
import Data.Bool(otherwise)
import Myocardio.Endo(Endo)

data Exercise = Exercise {
    name     :: Text
  , muscles  :: [Text]
  , reps     :: Text
  , category :: Text
  , last     :: Maybe UTCTime
  , tagged   :: Maybe UTCTime
  } deriving(Generic, Show, ToJSON, FromJSON, Eq)

toggleTag :: UTCTime -> Endo Exercise
toggleTag now ex | isJust (tagged ex) = ex { tagged = Nothing }
                 | otherwise = ex { tagged = Just now }
