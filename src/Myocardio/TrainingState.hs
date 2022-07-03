module Myocardio.TrainingState (TrainingState (Good, Medium, Bad)) where

import Data.Eq (Eq)
import Data.Ord (Ord)
import Text.Show (Show)
import Prelude ()

data TrainingState
  = Good
  | Medium
  | Bad
  deriving (Eq, Ord, Show)
