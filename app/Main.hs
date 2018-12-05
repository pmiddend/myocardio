{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Myocardio.AppState
import Myocardio.AppListType
import Control.Monad.IO.Class(liftIO)
import           Data.Functor                   ( (<$>) )
import           Data.Time.Clock                ( UTCTime
                                                , getCurrentTime
                                                )
import           Data.Foldable                  ( foldMap )
import           Prelude                        ( (+) )
import           Myocardio.Data                 ( Data(..), exercisesL )
import           Myocardio.FormatTime           ( formatTimeDiff )
import           Myocardio.Json                 ( readConfigFile )
import           Myocardio.Exercise             ( Exercise(..), toggleTag )
import           Myocardio.Ranking              ( reorderExercises )
import           Myocardio.Util                 ( textShow )
import           Myocardio.Endo                 ( Endo )
import           Myocardio.ExerciseId           ( ExerciseId
                                                , calculateIds
                                                )
import           Control.Applicative            ( pure )
import           System.IO                      ( IO )
import           Data.List                      ( zip
                                                , zipWith
                                                , lookup
                                                )
import           Brick.Main                     ( defaultMain
                                                , showFirstCursor
                                                , App(..)
                                                , continue
                                                , halt
                                                )
import           Brick.AttrMap                  ( AttrMap
                                                , AttrName
                                                , attrMap
                                                )
import           Brick.Widgets.List             ( listSelectedAttr, listMoveDown, listMoveUp,
                                                listAttr
                                                , listSelected
                                                , list
                                                , renderList
                                                )
import           Brick.Types                    ( Widget
                                                , BrickEvent(..)
                                                , EventM
                                                , Next
                                                )
import           Brick.Util                     ( fg
                                                , on
                                                )

import           Graphics.Vty                   ( white
                                                , blue
                                                , cyan
                                                , defAttr
                                                , Event(..)
                                                , Key(..)
                                                )
import           Data.Function                  ( const
                                                , ($)
                                                , (.)
                                                )
import           Data.Semigroup                 ( Semigroup(..) )
import           Data.Monoid                    ( (<>)
                                                , getSum
                                                , Sum(..)
                                                , Monoid(..)
                                                )
import           Control.Monad                  ( void )
import           Data.Ord                       ( max )
import           Brick.Widgets.Center           ( hCenter )
import           Brick.Widgets.Core             ( str
                                                , (<=>)
                                                )
import           Data.Maybe                     ( isJust
                                                , fromJust
                                                , Maybe(Just, Nothing)
                                                )
import qualified Data.Vector                   as Vec
import           Data.Bool                      ( Bool(..) )
import           Data.Int                       ( Int )
import           Data.Text                      ( Text
                                                , unwords
                                                , justifyLeft
                                                , unpack
                                                , length
                                                )
import Lens.Micro.Platform((&), (%~), ix, (^.))

newtype ColumnSizes = ColumnSizes {
  getColSizes :: [Int]
                                  }

colSizeApply :: (Int -> Int -> Int) -> ColumnSizes -> ColumnSizes -> ColumnSizes
colSizeApply f (ColumnSizes a) (ColumnSizes b) = ColumnSizes (zipWith f a b)

instance Semigroup ColumnSizes where
  (<>) = colSizeApply max

instance Monoid ColumnSizes where
  mempty = ColumnSizes [0..]

applySizes :: ColumnSizes -> [Text] -> Text
applySizes sizes cols =
  let just :: Int -> Text -> Text
      just i = justifyLeft (i + 1) ' '
  in  unwords (zipWith just (getColSizes sizes) cols)

listDrawElement
  :: UTCTime
  -> ColumnSizes
  -> [(ExerciseId, Exercise)]
  -> Bool
  -> ExerciseId
  -> Widget ()
listDrawElement now sizes exs isSelected exId =
  let ex :: Exercise
      ex = fromJust (lookup exId exs)
      lastStr :: Text
      lastStr = foldMap (formatTimeDiff now) (last ex)
      taggedStr :: Text
      taggedStr = if isJust (tagged ex) then "*" else " "
      row       = [name ex, reps ex, taggedStr, lastStr]
  in  str (unpack (applySizes sizes row))

headings :: [Text]
headings = ["Name", "Reps", "T", "Last"]

headingSizes :: ColumnSizes
headingSizes = ColumnSizes (length <$> headings)

colSizes :: UTCTime -> [Exercise] -> ColumnSizes
colSizes _ [] = mempty
colSizes now exs =
  let sizeSingle :: Exercise -> ColumnSizes
      sizeSingle ex = headingSizes <> ColumnSizes
        [ length (name ex)
        , length (reps ex)
        , 1
        , getSum . foldMap (Sum . length . formatTimeDiff now) . last $ ex
        ]
  in  foldMap sizeSingle exs

drawUI :: AppState -> [Widget ()]
drawUI state = [ui]
 where
  exs = state ^. stateData . exercisesL
  colSizes' = colSizes (state ^. stateNow) (state ^. stateData . exercisesL)
  ids = calculateIds exs
  box       = renderList
    (listDrawElement (state ^. stateNow) colSizes' (zip ids exs))
    True
    (state ^. stateList)
  ui = str (unpack (applySizes colSizes' headings)) <=> hCenter box


customAttr :: AttrName
customAttr = listSelectedAttr <> "custom"

theMap :: AttrMap
theMap = attrMap
  defAttr
  [ (listAttr        , white `on` blue)
  , (listSelectedAttr, blue `on` white)
  , (customAttr      , fg cyan)
  ]

modifyList :: Endo AppListType -> AppState -> EventM () (Next AppState)
modifyList f state = continue (state & stateList %~ f)

appEvent :: AppState -> BrickEvent () e -> EventM () (Next AppState)
appEvent l (VtyEvent e) = case e of
  EvKey (KChar 'j') [] -> modifyList listMoveDown l
  EvKey (KChar 'k') [] -> modifyList listMoveUp l
  EvKey (KChar 'q') [] -> halt l
  EvKey (KChar 't') [] -> 
    case listSelected (l ^. stateList) of
      Nothing -> continue l
      Just idx -> do
        now <- liftIO getCurrentTime
        let newState = l & stateData . exercisesL . ix idx %~ toggleTag now
        continue newState
  --   let el  = nextElement (listElements l)
  --       pos = Vec.length $ l ^. (listElementsL)
  --   in  continue $ listInsert pos el l

  -- EvKey (KChar '-') [] -> case l ^. (listSelectedL) of
  --   Nothing -> continue l
  --   Just i  -> continue $ listRemove i l
  EvKey KEsc [] -> halt l
  _ -> continue l

appEvent l _ = continue l

main :: IO ()
main = do
  configFile <- readConfigFile
  now        <- getCurrentTime
  let app = App
        { appDraw         = drawUI
        , appChooseCursor = showFirstCursor
        , appHandleEvent  = appEvent
        , appStartEvent   = pure
        , appAttrMap      = const theMap
        }
      exs          = reorderExercises (exercises configFile)
      ids = calculateIds exs
      initialState = AppState (list () (Vec.fromList ids) 1) configFile now
  void $ defaultMain app initialState
