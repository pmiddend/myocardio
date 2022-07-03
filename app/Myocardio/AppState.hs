{-# LANGUAGE TemplateHaskell #-}

module Myocardio.AppState
  ( AppState (..),
    stateTableCursor,
    stateEditor,
    stateData,
    stateNow,
    stateEditorFocus,
    statePage,
    AppPage(PageMain,PageMuscles)
  )
where

import Brick.Widgets.Edit (Editor)
import Data.Bool (Bool)
import Data.Int (Int)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Lens.Micro.Platform (makeLenses)
import Myocardio.Data (Data)
import Myocardio.ResourceName (ResourceName)
import Prelude ()

data AppPage = PageMain | PageMuscles

data AppState = AppState
  { _stateTableCursor :: Int,
    _stateEditor :: Editor Text ResourceName,
    _stateData :: Data,
    _statePage :: AppPage,
    _stateNow :: UTCTime,
    _stateEditorFocus :: Bool
  }

makeLenses ''AppState
