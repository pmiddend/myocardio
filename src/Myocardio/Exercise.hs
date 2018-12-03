{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Myocardio.Exercise where

import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           Data.Maybe                     ( Maybe )
import           Data.Text                      ( Text )
import           Data.Time.Clock                ( UTCTime )
import           GHC.Generics                   ( Generic )
import           Text.Show                      ( Show )
import           Data.Eq                        ( Eq )

data Exercise = Exercise {
    name     :: Text
  , muscles  :: [Text]
  , reps     :: Text
  , category :: Text
  , last     :: Maybe UTCTime
  , tagged   :: Maybe UTCTime
  } deriving(Generic, Show, ToJSON, FromJSON, Eq)
