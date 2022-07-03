{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Myocardio.Data(Data(..), categoriesL, exercisesL, emptyData) where

import Data.Aeson
  ( FromJSON,
    ToJSON,
  )
import Data.Text (Text)
import GHC.Generics (Generic)
import Lens.Micro.Platform (makeLensesFor)
import Myocardio.Exercise (Exercise)
import Text.Show (Show)

data Data = Data
  { categories :: [Text],
    exercises :: [Exercise]
  }
  deriving (Generic, Show, ToJSON, FromJSON)

makeLensesFor [("categories", "categoriesL"), ("exercises", "exercisesL")] ''Data

emptyData :: Data
emptyData = Data [] []
