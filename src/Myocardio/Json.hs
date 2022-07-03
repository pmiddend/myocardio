{-# LANGUAGE NoImplicitPrelude #-}

module Myocardio.Json(appName, configFileName, mkConfigDir, readConfigFile, writeConfigFile) where

import Control.Applicative (pure)
import Control.Exception (catchJust)
import Control.Monad (guard, (>>=))
import Data.Aeson (eitherDecodeFileStrict)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Bool (Bool (True))
import Data.ByteString.Lazy (writeFile)
import Data.Function ((.))
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
import Data.Either (Either(Right, Left))
import Prelude (error)

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
  maybeResult <- catchJust (guard . isDoesNotExistError) (eitherDecodeFileStrict fn) (\_ -> pure (Right emptyData))
  case maybeResult of
    Left e -> error e
    Right v -> pure v

writeConfigFile :: Data -> IO ()
writeConfigFile d = do
  mkConfigDir
  fn <- configFileName
  writeFile fn (encodePretty d)
