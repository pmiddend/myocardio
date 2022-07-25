{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module MyocardioApp.ConfigJson
  ( appName,
    configFileName,
    userL,
    urlL,
    webdavL,
    mkConfigDir,
    readConfigFile,
    writeDataFile,
    dataFileName,
    readDataFile,
    Config (..),
    ConfigWebdav (..),
  )
where

import Control.Applicative (pure)
import Control.Exception (catchJust)
import Control.Monad (guard, (>>=))
import Data.Aeson (FromJSON, eitherDecodeFileStrict)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Bool (Bool (True))
import Data.ByteString.Lazy (writeFile)
import Data.Either (Either (Left, Right))
import Data.Function ((.))
import Data.Maybe (Maybe (Nothing))
import Data.Text (Text)
import GHC.Generics (Generic)
import Lens.Micro.Platform (makeLensesFor)
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
import Text.Show (Show)
import Prelude (error)

data ConfigWebdav = ConfigWebdav
  { url :: Text,
    user :: Text
  }
  deriving (Generic, Show, FromJSON)

makeLensesFor [("url", "urlL"), ("user", "userL")] ''ConfigWebdav

data Config = Config
  { webdav :: Maybe ConfigWebdav
  }
  deriving (Generic, Show, FromJSON)

makeLensesFor [("webdav", "webdavL")] ''Config

emptyConfig :: Config
emptyConfig = Config Nothing

appName :: FilePath
appName = "myocardio"

dataFileName :: IO FilePath
dataFileName = getUserConfigFile appName "data.json"

configFileName :: IO FilePath
configFileName = getUserConfigFile appName "config.json"

mkConfigDir :: IO ()
mkConfigDir = getUserConfigDir appName >>= createDirectoryIfMissing True

readJsonFile :: FromJSON a => a -> IO FilePath -> IO a
readJsonFile default_ retrievePath = do
  mkConfigDir
  fn <- retrievePath
  maybeResult <- catchJust (guard . isDoesNotExistError) (eitherDecodeFileStrict fn) (\_ -> pure (Right default_))
  case maybeResult of
    Left e -> error e
    Right v -> pure v

readDataFile :: IO ExerciseData
readDataFile = readJsonFile emptyExerciseData dataFileName

readConfigFile :: IO Config
readConfigFile = readJsonFile emptyConfig configFileName

writeDataFile :: ExerciseData -> IO ()
writeDataFile d = do
  mkConfigDir
  fn <- dataFileName
  writeFile fn (encodePretty d)
