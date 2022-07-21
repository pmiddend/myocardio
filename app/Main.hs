{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import MyocardioApp.UpdateResult(UpdateResult(UpdateResultHalt, UpdateResultContinue))
import Brick.AttrMap
  ( AttrMap,
    attrMap,
  )
import Brick.Main
  ( App (App, appAttrMap, appChooseCursor, appDraw, appHandleEvent, appStartEvent),
    continue,
    defaultMain,
    halt,
  )
import Brick.Types
  ( BrickEvent (VtyEvent),
    CursorLocation,
    EventM,
    Next,
    Widget,
    cursorLocationName,
  )
import Brick.Util
  ( fg,
  )
import Control.Applicative (pure)
import Control.Monad (void)
import Data.Eq ((==))
import Data.Foldable (find)
import Data.Function
  ( const,
    ($),
    (.),
  )
import Data.Maybe
  ( Maybe,
  )
import Data.Time.Clock (getCurrentTime)
import Graphics.Vty
  ( Event (EvKey),
    Key (KChar, KEsc),
    cyan,
    defAttr,
  )
import Lens.Micro.Platform
  ( (&),
    (.~),
    (^.),
  )
import Myocardio.Json
  ( readConfigFile,
  )
import MyocardioApp.Page (Page (PageMain, PageMuscles))
import qualified MyocardioApp.Pages.MainPage as MainPage
import qualified MyocardioApp.Pages.MusclesPage as MusclesPage
import MyocardioApp.ResourceName (ResourceName)
import qualified Myocardio.TablePure as Table
import MyocardioApp.GlobalData(GlobalData(GlobalData))
import MyocardioApp.Model
  ( Model (Model), modelPage, modelGlobalData
  )
import System.IO (IO)
import Data.Semigroup ((<>))

-- log :: MonadIO m => Text -> m ()
-- log logText = do
--   now <- liftIO getCurrentTime
--   liftIO $ appendFile "/tmp/log.txt" (Text.pack (show now) <> ": " <> logText <> "\n")

view :: Model -> [Widget ResourceName]
view model =
  case model ^. modelPage of
    PageMain subModel -> MainPage.view subModel
    PageMuscles subModel -> MusclesPage.view subModel

theMap :: AttrMap
theMap =
  attrMap
    defAttr
    ((Table.selectedAttr, fg cyan) : (MusclesPage.attrs <> MainPage.attrs))

update ::
  Model ->
  BrickEvent ResourceName e ->
  EventM ResourceName (Next Model)
update model e =
  case e of
    VtyEvent (EvKey KEsc []) -> halt model
    VtyEvent (EvKey (KChar '\t') []) ->
      case model ^. modelPage of
        PageMain _ -> continue (model & modelPage .~ PageMuscles (MusclesPage.init (model ^. modelGlobalData)))
        PageMuscles _ -> continue (model & modelPage .~ PageMain (MainPage.init (model ^. modelGlobalData)))
    _ ->
      case model ^. modelPage of
        PageMain mainModel -> do
          updateResult <- MainPage.update mainModel e
          case updateResult of
            UpdateResultHalt newModel -> halt (model & modelPage .~ PageMain newModel)
            UpdateResultContinue newModel -> continue (model & modelPage .~ PageMain newModel)
        PageMuscles musclesModel -> do
          updateResult <- MusclesPage.update musclesModel e
          case updateResult of
            UpdateResultHalt newModel -> halt (model & modelPage .~ PageMuscles newModel)
            UpdateResultContinue newModel -> continue (model & modelPage .~ PageMuscles newModel)

appCursor ::
  Model ->
  [CursorLocation ResourceName] ->
  Maybe (CursorLocation ResourceName)
appCursor model cl =
  let filterList x = find ((== x) . cursorLocationName) cl
   in
    case model ^. modelPage of
      PageMain mainModel -> filterList (MainPage.cursorLocation mainModel)
      PageMuscles musclesModel -> filterList (MusclesPage.cursorLocation musclesModel)

main :: IO ()
main = do
  configFile <- readConfigFile
  now <- getCurrentTime
  let app =
        App
          { appDraw = view,
            appChooseCursor = appCursor,
            appHandleEvent = update,
            appStartEvent = pure,
            appAttrMap = const theMap
          }
      globalData = GlobalData configFile now
      initialState = Model globalData (PageMain (MainPage.init globalData))
  void $ defaultMain app initialState
