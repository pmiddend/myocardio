{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Myocardio.Human
  ( TrainingState (..),
    generateHumanMarkup,
    MuscleWithTrainingState (..),
    Muscle (..),
    FrontOrBack (..),
  )
where

import Brick (Widget)
import Data.List (intersperse)
import qualified Data.Map as M
import Data.Monoid (mconcat)
import Data.Text (Text)
import Lens.Micro (ix, (%~), (&))
import Myocardio.Markup (GetAttr, Markup, fromText, markup, markupSet)

data TrainingState = Good | Medium | Bad deriving (Eq, Ord, Show)

data Muscle = GluteusMaximus | Triceps | Neck deriving (Eq, Ord, Show)

data MuscleWithTrainingState = MuscleWithTrainingState
  { muscle :: Muscle,
    trainingState :: TrainingState
  }

type MuscleMap = M.Map Muscle TrainingState

type CharacterImage = [Text]

humanMatrixFront :: CharacterImage
humanMatrixFront =
  [ "         ______          ",
    "        / ......\\        ",
    "        |.^...^.|        ",
    "        @...>...@        ",
    "         \\..u../         ",
    "      ____|..|___        ",
    "    /.............\\      ",
    "   (...............)     ",
    "  (./ |..o....o..|\\.)    ",
    " (./   \\........./ \\.)   ",
    " |.|   |.__:__..|  |.|   ",
    " |.|   |.__:__..|  |.|   ",
    " |.|   |.__:__..|  |.|   ",
    " |.|   |........|  |.|   ",
    "  \"    |...__...|  \"     ",
    "       |..|  |..|        ",
    "       |..|  |..|        ",
    "       |..|  |..|        ",
    "       |x.|  |.x|        ",
    "       |..)  (..|        ",
    "       |..)  (..|        ",
    "       |..)  (..|        ",
    "      /...|  |...\\       ",
    "     /.../    \\...\\      "
  ]

humanMatrixBack :: CharacterImage
humanMatrixBack =
  [ "         ______          ",
    "        / ......\\        ",
    "        |.......|        ",
    "        @.......@        ",
    "         \\...../         ",
    "      ____|..|___        ",
    "    /.............\\      ",
    "   (...............)     ",
    "  (./ |..........|\\.)    ",
    " (./   \\........./ \\.)   ",
    " |.|   |........|  |.|   ",
    " |.|   |........|  |.|   ",
    " |.|   |........|  |.|   ",
    " |.|   |........|  |.|   ",
    "  \"    |..._|_..|  \"     ",
    "       |..|  |..|        ",
    "       |..|  |..|        ",
    "       |..|  |..|        ",
    "       |..|  |..|        ",
    "       |..)  (..|        ",
    "       |..)  (..|        ",
    "       |..)  (..|        ",
    "      /...|  |...\\       ",
    "     /.../    \\...\\      "
  ]

data LineSpan = LineSpan
  { yCoord :: Int,
    xCoordBeginInclusive :: Int,
    xCoordEndInclusive :: Int
  }

data FrontOrBack = Front | Back
  deriving (Eq)

characterImageForDirection Front = humanMatrixFront
characterImageForDirection Back = humanMatrixBack

data LineSpansWithFrontOrBack = LineSpansWithFrontOrBack
  { frontOrBack :: FrontOrBack,
    lineSpans :: [LineSpan]
  }

muscleToSpans :: Muscle -> LineSpansWithFrontOrBack
muscleToSpans GluteusMaximus = LineSpansWithFrontOrBack Back [LineSpan 14 8 11, LineSpan 14 13 15, LineSpan 15 8 11, LineSpan 15 13 15]
muscleToSpans Triceps = LineSpansWithFrontOrBack Back [LineSpan 8 4 4, LineSpan 9 3 3, LineSpan 10 2 2, LineSpan 8 18 18, LineSpan 9 19 19, LineSpan 10 20 20]
muscleToSpans Neck = LineSpansWithFrontOrBack Front [LineSpan 6 11 12]

applyToIdx :: Int -> (a -> a) -> [a] -> [a]
applyToIdx idx f xs = xs & ix idx %~ f

generateHumanMarkup :: forall a n. (Monoid a, GetAttr a, Eq a) => [MuscleWithTrainingState] -> (TrainingState -> a) -> FrontOrBack -> Widget n
generateHumanMarkup muscles trainingStateToMarkup direction =
  let humanImage = characterImageForDirection direction
      markuppedImage :: [Markup a]
      markuppedImage = fromText <$> humanImage
      -- Apply markup to specific line span in image
      spanTransducer :: a -> LineSpan -> [Markup a] -> [Markup a]
      spanTransducer trainingStateMarkup (LineSpan row colBegin colEnd) =
        applyToIdx (row-1) (markupSet (colBegin, colEnd - colBegin + 1) trainingStateMarkup)
      -- 1. unpack line spans for a single muscle
      -- 2. fold spans with image, fixing markup due to muscle state
      transducer :: MuscleWithTrainingState -> [Markup a] -> [Markup a]
      transducer (MuscleWithTrainingState muscle trainingState) image =
        let (LineSpansWithFrontOrBack muscleDirection spans) = muscleToSpans muscle
         in if muscleDirection /= direction
              then image
              else foldr (spanTransducer (trainingStateToMarkup trainingState)) image spans
      lines :: [Markup a]
      lines = intersperse (fromText "\n") (foldr transducer markuppedImage muscles)
   in markup (mconcat lines)
