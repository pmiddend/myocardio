{-# LANGUAGE NoImplicitPrelude #-}
module Myocardio.Json where

import           Control.Applicative            (pure)
import           Control.Monad                  ((>>=))
import           Data.Aeson                     (decodeFileStrict)
import           Data.Aeson.Encode.Pretty       (encodePretty)
import           Data.Bool                      (Bool (True))
import           Data.ByteString.Lazy           (writeFile)
import           Data.Maybe                     (fromJust)
import           Myocardio.Data                 (Data)
import           System.Directory               (createDirectoryIfMissing)
import           System.Environment.XDG.BaseDir (getUserConfigDir,
                                                 getUserConfigFile)
import           System.IO                      (FilePath, IO)

appName :: FilePath
appName = "myocardio"

configFileName :: IO FilePath
configFileName = getUserConfigFile appName "data.json"

mkConfigDir :: IO ()
mkConfigDir = getUserConfigDir appName >>= createDirectoryIfMissing True

readConfigFile :: IO Data
readConfigFile = do
  mkConfigDir
  fn <- configFileName
  maybeResult <- decodeFileStrict fn
  pure (fromJust maybeResult)

writeConfigFile :: Data -> IO ()
writeConfigFile d = do
  mkConfigDir
  fn <- configFileName
  writeFile fn (encodePretty d)
