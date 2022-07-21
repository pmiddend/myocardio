{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import Brick.AttrMap
  ( AttrMap,
    AttrName,
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
    Padding (Max),
    Widget,
    cursorLocationName,
  )
import Brick.Util
  ( bg,
    fg,
  )
import Brick.Widgets.Core (padRight, txt, withAttr, (<=>))
import Control.Applicative (pure)
import Control.Monad (void)
import Data.Eq ((==))
import Data.Foldable (find)
import Data.Function
  ( const,
    ($),
    (.),
  )
import Data.Functor ((<$>))
import Data.Maybe
  ( Maybe,
  )
import Data.Semigroup ((<>))
import Data.Time.Clock (getCurrentTime)
import Graphics.Vty
  ( Event (EvKey),
    Key (KEsc, KFun),
    bold,
    brightCyan,
    cyan,
    defAttr,
    withStyle,
  )
import Lens.Micro.Platform
  ( (&),
    (.~),
    (^.),
  )
import Myocardio.Json
  ( readConfigFile,
  )
import MyocardioApp.BrickUtil (stackHorizontal)
import MyocardioApp.GlobalData (GlobalData (GlobalData))
import MyocardioApp.Model
  ( Model (Model),
    modelGlobalData,
    modelPage,
  )
import MyocardioApp.Page (Page (PageMain, PageMuscles), isPageMain, isPageMuscles)
import qualified MyocardioApp.Pages.MainPage as MainPage
import qualified MyocardioApp.Pages.MusclesPage as MusclesPage
import MyocardioApp.ResourceName (ResourceName)
import qualified MyocardioApp.TablePure as Table
import MyocardioApp.UpdateResult (UpdateResult (UpdateResultContinue, UpdateResultHalt))
import System.IO (IO)

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
    VtyEvent (EvKey (KFun 1) []) ->
      continue (model & modelPage .~ PageMain (MainPage.init (model ^. modelGlobalData)))
    VtyEvent (EvKey (KFun 2) []) ->
      case model ^. modelPage of
        PageMuscles _ -> continue model
        PageMain mainModel -> continue (model & modelPage .~ PageMuscles (MusclesPage.init (MainPage.deinit mainModel)))
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
   in case model ^. modelPage of
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
