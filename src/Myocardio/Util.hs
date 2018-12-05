module Myocardio.Util where

import Prelude()
import Data.Function((.))
import           Text.Show                      ( Show
                                                , show
                                                )
import           Data.Text                      ( Text
                                                , pack
                                                )

textShow :: Show a => a -> Text
textShow = pack . show
