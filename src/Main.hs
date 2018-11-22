{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import           Myocardio.Data                 ( Data(..) )
import           Myocardio.Json                 ( readConfigFile )
import           Myocardio.Ranking                 ( rankTime )
import           System.IO                      ( IO
                                                , print
                                                )
import Data.List(zip)

main :: IO ()
main = do
  cf <- readConfigFile
  let exs = exercises cf
  print (zip exs (rankTime exs))
