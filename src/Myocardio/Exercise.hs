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

data Exercise = Exercise {
    name     :: Text
  , muscles  :: [Text]
  , reps     :: Text
  , category :: Text
  , dates    :: [UTCTime]
  , tagged   :: Maybe UTCTime
  } deriving(Generic, Show, ToJSON, FromJSON)
