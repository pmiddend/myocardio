{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE TemplateHaskell    #-}
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
import Data.Bool(otherwise, Bool)
import Myocardio.Endo(Endo)
import Data.Function((.))
import Lens.Micro.Platform(makeLensesFor)

data Exercise = Exercise {
    name     :: Text
  , muscles  :: [Text]
  , reps     :: Text
  , category :: Text
  , last     :: Maybe UTCTime
  , tagged   :: Maybe UTCTime
  } deriving(Generic, Show, ToJSON, FromJSON, Eq)

makeLensesFor [("name", "nameL"), ("muscles", "musclesL"),("reps", "repsL"),("category", "categoryL"),("last", "lastL"),("tagged", "taggedL")] ''Exercise

isTagged :: Exercise -> Bool
isTagged = isJust . tagged

toggleTag :: UTCTime -> Endo Exercise
toggleTag now ex | isTagged ex = ex { tagged = Nothing }
                 | otherwise = ex { tagged = Just now }

commit :: UTCTime -> Endo Exercise
commit now ex | isTagged ex = ex { tagged = Nothing, last = Just now }
              | otherwise = ex
