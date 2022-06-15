module Myocardio.Util where

import Data.Function ((.))
import Data.Text
  ( Text,
    pack,
  )
import Text.Show
  ( Show,
    show,
  )
import Prelude ()

textShow :: Show a => a -> Text
textShow = pack . show
