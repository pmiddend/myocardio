{-# LANGUAGE TemplateHaskell #-}

module MyocardioApp.Model
  ( Model (..),
    modelGlobalData,
    modelPage
  )
where

import Lens.Micro.Platform (makeLenses)
import MyocardioApp.GlobalData (GlobalData)
import Prelude ()
import MyocardioApp.Page(Page)

data Model = Model
  { _modelGlobalData :: GlobalData,
    _modelPage :: Page
  }

makeLenses ''Model
