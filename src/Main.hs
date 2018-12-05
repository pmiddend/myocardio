{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Functor                   ( (<$>) )
import           Data.Tuple                     ( fst
                                                , snd
                                                )
import           Data.Time.Clock                ( UTCTime
                                                , getCurrentTime
                                                )
import           Data.Foldable                  ( foldMap )
import           Prelude                        ( (+) )
import           Myocardio.Data                 ( Data(..) )
import           Myocardio.FormatTime           ( formatTimeDiff )
import           Myocardio.Json                 ( readConfigFile )
import           Myocardio.Exercise             ( Exercise(..) )
import           Myocardio.Ranking              ( reorderExercises )
import           Myocardio.Util                 ( textShow )
import           Myocardio.ExerciseId           ( ExerciseId
                                                , calculateIds
                                                , exerciseIdLength
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
import           Brick.Widgets.List             ( listSelectedAttr
                                                , listAttr
                                                , list
                                                , handleListEvent
                                                , List
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

newtype ColumnSizes = ColumnSizes {
  getColSizes :: [Int]
                                  }

colSizeApply :: (Int -> Int -> Int) -> ColumnSizes -> ColumnSizes -> ColumnSizes
colSizeApply f (ColumnSizes a) (ColumnSizes b) = ColumnSizes (zipWith f a b)

instance Semigroup ColumnSizes where
  (<>) = colSizeApply max

instance Monoid ColumnSizes where
  mempty = ColumnSizes [0..]

data AppState = AppState {
  stateList :: List () ExerciseId
  , stateExercises :: [(ExerciseId, Exercise)]
  , stateNow :: UTCTime
  }

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
      row       = [textShow exId, name ex, reps ex, taggedStr, lastStr]
  in  str (unpack (applySizes sizes row))

headings :: [Text]
headings = ["ID", "Name", "Reps", "T", "Last"]

headingSizes :: ColumnSizes
headingSizes = ColumnSizes (length <$> headings)

colSizes :: UTCTime -> [Exercise] -> ColumnSizes
colSizes _ [] = mempty
colSizes now exs =
  let ids = calculateIds exs
      sizeSingle :: (Exercise, ExerciseId) -> ColumnSizes
      sizeSingle (ex, exid) = headingSizes <> ColumnSizes
        [ exerciseIdLength exid
        , length (name ex)
        , length (reps ex)
        , 1
        , getSum . foldMap (Sum . length . formatTimeDiff now) . last $ ex
        ]
  in  foldMap sizeSingle (zip exs ids)

drawUI :: AppState -> [Widget ()]
drawUI state = [ui]
 where
  colSizes' = colSizes (stateNow state) (snd <$> stateExercises state)
  box       = renderList
    (listDrawElement (stateNow state) colSizes' (stateExercises state))
    True
    (stateList state)
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


appEvent :: AppState -> BrickEvent () e -> EventM () (Next AppState)
appEvent l (VtyEvent e) = case e of
  -- EvKey (KChar '+') [] ->
  --   let el  = nextElement (listElements l)
  --       pos = Vec.length $ l ^. (listElementsL)
  --   in  continue $ listInsert pos el l

  -- EvKey (KChar '-') [] -> case l ^. (listSelectedL) of
  --   Nothing -> continue l
  --   Just i  -> continue $ listRemove i l
  EvKey KEsc [] -> halt l

  ev            -> do
    newList <- handleListEvent ev (stateList l)
    continue (l { stateList = newList })
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
      exsWithIds   = zip (calculateIds exs) exs
      initialState = AppState (list () (Vec.fromList (fst <$> exsWithIds)) 1)
                              exsWithIds
                              now
  void $ defaultMain app initialState
