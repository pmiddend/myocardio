{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Myocardio.Exercise where

import Data.Aeson
  ( FromJSON,
    ToJSON,
  )
import Data.Bool (Bool, otherwise)
import Data.Eq (Eq)
import Data.Function ((.))
import Data.Maybe (Maybe (Just, Nothing), isJust)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)
import Lens.Micro.Platform (makeLensesFor)
import Myocardio.Endo (Endo)
import Text.Show (Show)

data Exercise = Exercise
  { name :: Text,
    muscles :: [Text],
    reps :: Text,
    category :: Text,
    last :: Maybe UTCTime,
    tagged :: Maybe UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, Eq)

makeLensesFor [("name", "nameL"), ("muscles", "musclesL"), ("reps", "repsL"), ("category", "categoryL"), ("last", "lastL"), ("tagged", "taggedL")] ''Exercise

isTagged :: Exercise -> Bool
isTagged = isJust . tagged

toggleTag :: UTCTime -> Endo Exercise
toggleTag now ex
  | isTagged ex = ex {tagged = Nothing}
  | otherwise = ex {tagged = Just now}

commit :: Endo Exercise
commit ex =
  case tagged ex of
    Nothing -> ex
    Just t -> ex {tagged = Nothing, last = Just t}
