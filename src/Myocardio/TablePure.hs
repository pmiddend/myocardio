{-# LANGUAGE OverloadedStrings #-}

module Myocardio.TablePure (Table, render, selectedAttr, RowBorderStyle (..), Alignments (..), Borders (..), Headings, Rows, CursorPosition, handleEvent, ColumnAlignment (..), RowAlignment (..)) where

import Brick (AttrName, BrickEvent (VtyEvent), EventM, Padding (Max, Pad), Result (image), Size (Fixed), ViewportType (Both), Widget (Widget), hBox, hLimit, joinBorders, padLeft, padTop, vBox, vLimit, visible, withAttr)
import qualified Brick
import Brick.Widgets.Border
import Brick.Widgets.Center (hCenter, vCenter)
import Brick.Widgets.Core (txt, viewport)
import qualified Brick.Widgets.Table as BrickTable
import qualified Control.Exception as E
import Control.Monad
import Data.List (intersperse, transpose)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as Text
import Graphics.Vty (Event (EvKey), Key (KChar), imageHeight, imageWidth)
import Lens.Micro.Platform (Getting, to)
import Prelude hiding (init)

data ColumnAlignment
  = -- | Align all cells to the left.
    AlignLeft
  | -- | Center the content horizontally in all cells in the column.
    AlignCenter
  | -- | Align all cells to the right.
    AlignRight
  deriving (Eq, Show, Read)

-- | Row alignment modes.
data RowAlignment
  = -- | Align all cells to the top.
    AlignTop
  | -- | Center the content vertically in all cells in the row.
    AlignMiddle
  | -- | Align all cells to the bottom.
    AlignBottom
  deriving (Eq, Show, Read)

-- | A table creation exception.
data TableException
  = -- | Rows did not all have the same number of cells.
    TEUnequalRowSizes
  | -- | Some cells in the table did not use the 'Fixed' size policy for
    -- both horizontal and vertical sizing.
    TEInvalidCellSizePolicy
  deriving (Eq, Show, Read)

instance E.Exception TableException

data RowBorderStyle = OnlyHeader | All | None

data Table = Table
  { headings :: [Text],
    rows :: [[Text]],
    cursorPosition :: Int
  }

data Alignments = Alignments
  { columnAlignments :: M.Map Int ColumnAlignment,
    rowAlignments :: M.Map Int RowAlignment,
    defaultColumnAlignment :: ColumnAlignment,
    defaultRowAlignment :: RowAlignment
  }

data Borders = Borders
  { drawSurroundingBorder :: Bool,
    rowBorderStyle :: RowBorderStyle,
    drawColumnBorders :: Bool
  }

currentRow :: Getting r Table Int
currentRow = to cursorPosition

moveDown :: Table -> Table
moveDown t = t {cursorPosition = (cursorPosition t + 1) `mod` length (rows t)}

moveUp :: Table -> Table
moveUp t = t {cursorPosition = max 0 (cursorPosition t - 1)}

handleEvent :: BrickEvent n e -> Table -> EventM n (Maybe Table)
handleEvent e table =
  case e of
    VtyEvent (EvKey (KChar 'j') []) -> pure (Just (moveDown table))
    VtyEvent (EvKey (KChar 'k') []) -> pure (Just (moveUp table))
    _ -> pure Nothing

selectedAttr :: AttrName
selectedAttr = "tableSelected"

insertAt :: Int -> a -> [a] -> [a]
insertAt idx e xs =
  let (prior, after) = splitAt idx xs
   in prior ++ [e] ++ after

imageHeightNonEmpty im = max 1 (imageHeight im)
imageWidthNonEmpty im = max 1 (imageWidth im)

renderTable' :: Alignments -> Borders -> [Widget n] -> [[Widget n]] -> Widget n
renderTable' alignment borders header rows =
  joinBorders $
    Widget Fixed Fixed $ do
      let allRows = header : rows
      cellResults <- forM allRows $ mapM Brick.render
      let rowHeights = rowHeight <$> cellResults
          colWidths = colWidth <$> byColumn
          allRowAligns =
            (\i -> M.findWithDefault (defaultRowAlignment alignment) i (rowAlignments alignment))
              <$> [0 .. length rowHeights - 1]
          allColAligns =
            (\i -> M.findWithDefault (defaultColumnAlignment alignment) i (columnAlignments alignment))
              <$> [0 .. length byColumn - 1]
          rowHeight = maximum . fmap (imageHeightNonEmpty . image)
          colWidth = maximum . fmap (imageWidthNonEmpty . image)
          byColumn = transpose cellResults
          toW = Widget Fixed Fixed . return
          totalHeight = sum rowHeights
          applyColAlignment align width w =
            Widget Fixed Fixed $ do
              result <- Brick.render w
              case align of
                AlignLeft -> return result
                AlignCenter -> Brick.render $ hLimit width $ hCenter $ toW result
                AlignRight ->
                  Brick.render $
                    padLeft (Pad (width - imageWidth (image result))) $
                      toW result
          applyRowAlignment rHeight align result =
            case align of
              AlignTop -> toW result
              AlignMiddle -> vLimit rHeight $ vCenter $ toW result
              AlignBottom -> vLimit rHeight $ padTop Max $ toW result
          mkColumn (hAlign, width, colCells) = do
            let paddedCells = flip map (zip3 allRowAligns rowHeights colCells) $ \(vAlign, rHeight, cell) ->
                  applyColAlignment hAlign width $
                    applyRowAlignment rHeight vAlign cell
                maybeRowBorders = case rowBorderStyle borders of
                  OnlyHeader -> insertAt 1 (hLimit width hBorder)
                  All -> intersperse (hLimit width hBorder)
                  None -> id
            Brick.render $ vBox $ maybeRowBorders paddedCells
      columns <- mapM mkColumn $ zip3 allColAligns colWidths byColumn
      let maybeColumnBorders =
            if drawColumnBorders borders
              then
                let rowBorderHeight = case rowBorderStyle borders of
                      OnlyHeader -> 1
                      All -> length rows - 1
                      None -> 0
                 in intersperse (vLimit (totalHeight + rowBorderHeight) vBorder)
              else id
      Brick.render $ hBox $ maybeColumnBorders $ toW <$> columns

type Headings = [Text]

type Rows = [[Text]]

type CursorPosition = Int

spaceIfEmpty :: Text -> Text
spaceIfEmpty n | Text.null n = " "
spaceIfEmpty n = n
               

render :: (Ord n, Show n) => n -> Headings -> Rows -> CursorPosition -> Alignments -> Borders -> Widget n
render viewportName headings rows cursorPosition alignments borders = viewport viewportName Both renderedTable
  where
    compiledRows = zipWith makeRow [0 ..] rows
    selectedTxt = withAttr selectedAttr . txt
    makeRow :: Int -> [Text] -> [Widget n]
    makeRow i cols = case cols of
      [] -> []
      head : tail ->
        if i == cursorPosition
          then visible (selectedTxt (spaceIfEmpty head)) : (selectedTxt . spaceIfEmpty <$> tail)
          else txt <$> cols
    renderedTable = renderTable' alignments borders (txt <$> headings) compiledRows
