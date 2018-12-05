{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Myocardio.Data where

import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )
import           Myocardio.Exercise             ( Exercise )
import           Text.Show                      ( Show )
import Lens.Micro.Platform(makeLensesFor)

data Data = Data {
    categories :: [Text]
  , muscles    :: [Text]
  , exercises  :: [Exercise]
  }  deriving(Generic, Show, ToJSON, FromJSON)

makeLensesFor [("categories", "categoriesL"), ("muscles", "musclesL"),("exercises", "exercisesL")] ''Data
