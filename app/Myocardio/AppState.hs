{-# LANGUAGE TemplateHaskell #-}
module Myocardio.AppState where

import Myocardio.Data(Data)
import Data.Time.Clock(UTCTime)
import Myocardio.AppListType(AppListType)
import Myocardio.ResourceName(ResourceName)
import Lens.Micro.Platform(makeLenses)
import Brick.Widgets.Edit(Editor)
import Data.Text(Text)

data AppState = AppState {
  _stateList :: AppListType
  , _stateEditor :: Editor Text ResourceName
  , _stateData :: Data
  , _stateNow :: UTCTime
  , _stateEditorFocus :: Bool
  }

makeLenses ''AppState
