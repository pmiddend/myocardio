{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Maybe         (Maybe (Nothing))
import           Myocardio.Data     (Data (..))
import           Myocardio.Exercise (Exercise (..))
import           Myocardio.Json     (writeConfigFile)
import           System.IO          (IO, putStrLn)

main :: IO ()
main = do
  let d = Data ["endurance", "normal", "stretch"] ["glute medius", "abs", "lower back"] [Exercise "Glute Medius Leg Raises" ["glute medius"] "30" "normal" [] Nothing]
  writeConfigFile d
