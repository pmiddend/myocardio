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
import Myocardio.ExerciseData (ExerciseData, emptyExerciseData)
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

readConfigFile :: IO ExerciseData
readConfigFile = do
  mkConfigDir
  fn <- configFileName
  maybeResult <- catchJust (guard . isDoesNotExistError) (eitherDecodeFileStrict fn) (\_ -> pure (Right emptyExerciseData))
  case maybeResult of
    Left e -> error e
    Right v -> pure v

writeConfigFile :: ExerciseData -> IO ()
writeConfigFile d = do
  mkConfigDir
  fn <- configFileName
  writeFile fn (encodePretty d)
