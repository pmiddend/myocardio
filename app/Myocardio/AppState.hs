{-# LANGUAGE TemplateHaskell #-}

module Myocardio.AppState where

import Brick.Widgets.Edit (Editor)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Lens.Micro.Platform (makeLenses)
import Myocardio.Data (Data)
import Myocardio.ResourceName (ResourceName)

data AppState = AppState
  { _stateTableCursor :: Int,
    _stateEditor :: Editor Text ResourceName,
    _stateData :: Data,
    _stateNow :: UTCTime,
    _stateEditorFocus :: Bool
  }

makeLenses ''AppState
