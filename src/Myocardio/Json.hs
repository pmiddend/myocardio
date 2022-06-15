{-# LANGUAGE NoImplicitPrelude #-}

module Myocardio.Json where

import Control.Applicative (pure)
import Control.Exception (catchJust)
import Control.Monad (guard, (>>=))
import Data.Aeson (decodeFileStrict)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Bool (Bool (True))
import Data.ByteString.Lazy (writeFile)
import Data.Function ((.))
import Data.Maybe (Maybe (Nothing), fromJust, fromMaybe)
import Myocardio.Data (Data, emptyData)
import System.Directory (createDirectoryIfMissing)
import System.Environment.XDG.BaseDir
  ( getUserConfigDir,
    getUserConfigFile,
  )
import System.IO
  ( FilePath,
    IO,
  )
import System.IO.Error (isDoesNotExistError)

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
  maybeResult <- catchJust (guard . isDoesNotExistError) (decodeFileStrict fn) (\_ -> pure Nothing)
  pure (fromMaybe emptyData maybeResult)

writeConfigFile :: Data -> IO ()
writeConfigFile d = do
  mkConfigDir
  fn <- configFileName
  writeFile fn (encodePretty d)
