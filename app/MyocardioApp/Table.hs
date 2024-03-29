{-# LANGUAGE OverloadedStrings #-}

module MyocardioApp.Table
  ( render,
    selectedAttr,
    RowBorderStyle (..),
    Alignments (..),
    Borders (..),
    Headings,
    Rows,
    CursorPosition,
    ColumnAlignment (..),
    RowAlignment (..),
  )
where

import Brick (AttrName, Padding (Max, Pad), Result (image), Size (Fixed, Greedy), ViewportType (Both), Widget (Widget, vSize), hBox, hLimit, joinBorders, padLeft, padTop, vBox, vLimit, visible, withAttr, getContext, imageL, attrL, (<=>))
import qualified Brick
import Brick.Widgets.Border (hBorder, vBorder)
import Brick.Widgets.Center (hCenter, vCenter)
import Brick.Widgets.Core (txt, viewport)
import qualified Control.Exception as E
import Control.Monad (Functor (fmap), forM, mapM)
import Data.Bool (Bool)
import Data.Eq (Eq ((==), (/=)))
import Data.Function (flip, id)
import Data.Int (Int)
import Data.List (intersperse, map, splitAt, transpose, zip3, zipWith, (++), length, take, drop)
import Lens.Micro((^.), to, (&), (.~))
import qualified Data.Map as M
import Data.Ord (Ord (max))
import Data.Text (Text)
import qualified Data.Text as Text
import Graphics.Vty (imageHeight, imageWidth, charFill, horizCat)
import Text.Read (Read)
import Text.Show (Show)
import Prelude (Applicative (pure), Foldable (maximum, sum), Num ((+), (-)), ($), (.), (<$>))

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

selectedAttr :: AttrName
selectedAttr = "tableSelected"

insertAt :: Int -> a -> [a] -> [a]
insertAt idx e xs =
  let (prior, after) = splitAt idx xs
   in prior ++ [e] ++ after

imageHeightNonEmpty im = max 1 (imageHeight im)

imageWidthNonEmpty im = max 1 (imageWidth im)

-- columnWidths allRows = do
--   cellResults <- forM allRows $ mapM Brick.render
--   let byColumn = transpose cellResults
--       colWidths = colWidth <$> byColumn

data TableRenderMode = RenderOnlyHeader | RenderOnlyBody | RenderBoth
  deriving(Eq)

padTo :: Int -> Widget n -> Widget n
padTo width p =
  Widget Greedy (vSize p) $ do
    result <- Brick.render p
    c <- getContext
    let rWidth = result ^. imageL . to imageWidth
        rHeight = result^.imageL.to imageHeight
        rightPaddingAmount = max 0 (width - rWidth)
        rightPadding = charFill (c^.attrL) ' ' rightPaddingAmount rHeight
        paddedImage = horizCat [ result^.imageL
                               , rightPadding
                               ]
    if rightPaddingAmount == 0
      then pure result
      else pure $ result & imageL .~ paddedImage

renderTable' :: TableRenderMode -> Alignments -> Borders -> [Widget n] -> [[Widget n]] -> Widget n
renderTable' renderMode alignment borders header rows' =
  joinBorders $
    Widget Fixed Fixed $ do
      let allRows = header : rows'
      cellResults <- forM allRows $ mapM Brick.render
      let rowHeights = rowHeight <$> cellResults
          byColumn = transpose cellResults
          truncatedColumns = case renderMode of
            RenderBoth -> byColumn
            RenderOnlyHeader -> take 1 <$> byColumn
            RenderOnlyBody -> drop 1 <$> byColumn
          colWidths = colWidth <$> byColumn
          allRowAligns =
            (\i -> M.findWithDefault (defaultRowAlignment alignment) i (rowAlignments alignment))
              <$> [0 .. length rowHeights - 1]
          allColAligns =
            (\i -> M.findWithDefault (defaultColumnAlignment alignment) i (columnAlignments alignment))
              <$> [0 .. length byColumn - 1]
          rowHeight = maximum . fmap (imageHeightNonEmpty . image)
          colWidth = maximum . fmap (imageWidthNonEmpty . image)
          toW = Widget Fixed Fixed . pure
          totalHeight =
            case renderMode of
              RenderOnlyBody -> sum rowHeights - 1
              RenderOnlyHeader -> 1
              RenderBoth -> sum rowHeights
          applyColAlignment align width w =
            Widget Fixed Fixed $
              case align of
                AlignLeft -> Brick.render $ padTo width w
                AlignCenter -> do
                  result <- Brick.render w
                  Brick.render $ hLimit width $ hCenter $ toW result
                AlignRight -> do
                  result <- Brick.render w
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
                  OnlyHeader -> if renderMode /= RenderOnlyBody then insertAt 1 (hLimit width hBorder) else id
                  All -> intersperse (hLimit width hBorder)
                  None -> id
            Brick.render $ vBox $ maybeRowBorders paddedCells
      columns <- mapM mkColumn $ zip3 allColAligns colWidths truncatedColumns
      let 
          maybeColumnBorders =
            if drawColumnBorders borders
              then
                let rowBorderHeight = case rowBorderStyle borders of
                      OnlyHeader -> 1
                      All -> length rows' - 1
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
render viewportName headings' rows' cursorPosition' alignments borders = renderedHeader <=> viewport viewportName Both renderedBody
  where
    compiledRows = zipWith makeRow [0 ..] rows'
    selectedTxt = withAttr selectedAttr . txt
    makeRow :: Int -> [Text] -> [Widget n]
    makeRow i cols = case cols of
      [] -> []
      head : tail ->
        if i == cursorPosition'
          then visible (selectedTxt (spaceIfEmpty head)) : (selectedTxt . spaceIfEmpty <$> tail)
          else txt <$> cols
    renderedHeader = renderTable' RenderOnlyHeader alignments borders (txt <$> headings') compiledRows
    renderedBody = renderTable' RenderOnlyBody alignments borders (txt <$> headings') compiledRows
