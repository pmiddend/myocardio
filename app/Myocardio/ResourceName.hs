module Myocardio.ResourceName where

import Data.Eq (Eq)
import Data.Ord (Ord)
import Text.Show (Show)
import Prelude (Bounded)

data ResourceName = NameEditor | NameList deriving (Show, Eq, Ord, Bounded)
