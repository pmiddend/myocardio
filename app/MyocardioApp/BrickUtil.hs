module MyocardioApp.BrickUtil(stackVertical, stackHorizontal) where

import Prelude()
import Brick.Types (Widget)
import Brick.Widgets.Core ((<+>), emptyWidget, (<=>))
import Data.Foldable (foldr)

stackVertical :: [Widget n] -> Widget n
stackVertical = foldr (<=>) emptyWidget

stackHorizontal :: [Widget n] -> Widget n
stackHorizontal = foldr (<+>) emptyWidget
