{-# LANGUAGE TemplateHaskell #-}

module Myocardio.AppState(AppState(..), stateTableCursor, stateEditor, stateData, stateNow, stateEditorFocus) where

import Brick.Widgets.Edit (Editor)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Lens.Micro.Platform (makeLenses)
import Myocardio.Data (Data)
import Myocardio.ResourceName (ResourceName)
import Prelude()
import Data.Int(Int)
import Data.Bool(Bool)

data AppState = AppState
  { _stateTableCursor :: Int,
    _stateEditor :: Editor Text ResourceName,
    _stateData :: Data,
    _stateNow :: UTCTime,
    _stateEditorFocus :: Bool
  }

makeLenses ''AppState
