{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import MyocardioApp.UpdateResult(UpdateResult(UpdateResultHalt, UpdateResultContinue))
import Brick.AttrMap
  ( AttrMap,
    attrMap, AttrName,
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
    cursorLocationName, Padding (Max),
  )
import Brick.Util
  ( fg, bg,
  )
import Control.Applicative (pure)
import Control.Monad (void)
import Data.Eq ((==))
import Data.Foldable (find)
import Data.Function
  ( const,
    ($),
    (.), id,
  )
import Data.Maybe
  ( Maybe,
  )
import Data.Time.Clock (getCurrentTime)
import Graphics.Vty
  ( Event (EvKey),
    Key (KChar, KEsc, KFun),
    cyan,
    defAttr, brightCyan, bold, withStyle,
  )
import Lens.Micro.Platform
  ( (&),
    (.~),
    (^.),
  )
import Myocardio.Json
  ( readConfigFile,
  )
import MyocardioApp.Page (Page (PageMain, PageMuscles), isPageMain, isPageMuscles)
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
import Brick.Widgets.Core (txt, (<=>), padRight, (<+>), withAttr)
import MyocardioApp.BrickUtil (stackHorizontal)
import Data.Functor ((<$>))

-- log :: MonadIO m => Text -> m ()
-- log logText = do
--   now <- liftIO getCurrentTime
--   liftIO $ appendFile "/tmp/log.txt" (Text.pack (show now) <> ": " <> logText <> "\n")

tabBarActiveAttr :: AttrName
tabBarActiveAttr = "tab bar active"

tabBarInactiveAttr :: AttrName
tabBarInactiveAttr = "tab bar inactive"

view :: Model -> [Widget ResourceName]
view model =
  let tabs = [("[F1]: Exercise List", isPageMain), ("[F2]: Muscles", isPageMuscles)]
      tabToWidget (tab, tabEnabled) = withAttr (if tabEnabled (model ^. modelPage) then tabBarActiveAttr else tabBarInactiveAttr) (padRight Max (txt tab))
      tabBar = stackHorizontal (tabToWidget <$> tabs)
  in case model ^. modelPage of
    PageMain subModel -> [tabBar <=> MainPage.view subModel]
    PageMuscles subModel -> [tabBar <=> MusclesPage.view subModel]

theMap :: AttrMap
theMap =
  attrMap
    defAttr
    ((Table.selectedAttr, fg cyan) : (tabBarActiveAttr, bg brightCyan `withStyle` bold) : (tabBarInactiveAttr, bg brightCyan) : (MusclesPage.attrs <> MainPage.attrs))

update ::
  Model ->
  BrickEvent ResourceName e ->
  EventM ResourceName (Next Model)
update model e =
  case e of
    VtyEvent (EvKey KEsc []) -> halt model
    VtyEvent (EvKey (KFun 1) []) -> continue (model & modelPage .~ PageMain (MainPage.init (model ^. modelGlobalData)))
    VtyEvent (EvKey (KFun 2) []) -> continue (model & modelPage .~ PageMuscles (MusclesPage.init (model ^. modelGlobalData)))
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
