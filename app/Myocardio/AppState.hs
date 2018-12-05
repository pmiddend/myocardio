{-# LANGUAGE TemplateHaskell #-}
module Myocardio.AppState where

import Myocardio.Data(Data)
import Data.Time.Clock(UTCTime)
import Myocardio.AppListType(AppListType)
import Lens.Micro.Platform(makeLenses)

data AppState = AppState {
  _stateList :: AppListType
  , _stateData :: Data
  , _stateNow :: UTCTime
  }

makeLenses ''AppState
