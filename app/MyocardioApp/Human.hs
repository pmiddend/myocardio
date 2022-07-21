{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MyocardioApp.Human
  ( generateHumanMarkup,
    MuscleWithTrainingState (..),
    Muscle (..),
    FrontOrBack (..),
  )
where

import Brick (Widget)
import Data.Eq (Eq ((/=)))
import Data.Functor ((<$>))
import Data.Int (Int)
import Data.List (intersperse)
import Data.Monoid (Monoid (mconcat))
import Data.Text (Text)
import Lens.Micro (ix, (%~), (&))
import MyocardioApp.Markup (GetAttr, Markup, fromText, markup, markupSet)
import Myocardio.Muscle
  ( Muscle
      ( BackQuadriceps,
        Calves,
        Core,
        Delta,
        GluteusMaximus,
        GluteusMedius,
        Hamstrings,
        HipFlexor,
        LowerBack,
        Neck,
        Pecs,
        Quadriceps,
        Rotators,
        SideCore,
        Triceps,
        UpperBack
      ),
  )
import Prelude (Foldable (foldr), Num ((+), (-)))
import Myocardio.TrainingState (TrainingState)
import Myocardio.MuscleWithTrainingState (MuscleWithTrainingState(MuscleWithTrainingState))


type CharacterImage = [Text]

humanMatrixFront :: CharacterImage
humanMatrixFront =
  [ " Front   ______          ",
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
    "       |x.|  |.x|        ",
    "       |..)  (..|        ",
    "       |..)  (..|        ",
    "       |..)  (..|        ",
    "      /...|  |...\\       "
  ]

humanMatrixBack :: CharacterImage
humanMatrixBack =
  [ " Back    ______          ",
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
    "       |..)  (..|        ",
    "       |..)  (..|        ",
    "       |..)  (..|        ",
    "      /...|  |...\\       "
  ]

data LineSpan = LineSpan
  { _yCoord :: Int,
    _xCoordBeginInclusive :: Int,
    _xCoordEndInclusive :: Int
  }

data FrontOrBack = Front | Back
  deriving (Eq)

characterImageForDirection Front = humanMatrixFront
characterImageForDirection Back = humanMatrixBack

data LineSpansWithFrontOrBack = LineSpansWithFrontOrBack
  { _frontOrBack :: FrontOrBack,
    _lineSpans :: [LineSpan]
  }

muscleToSpans :: Muscle -> LineSpansWithFrontOrBack
muscleToSpans GluteusMaximus = LineSpansWithFrontOrBack Back [LineSpan 14 8 11, LineSpan 14 13 15, LineSpan 15 8 11, LineSpan 15 13 15]
muscleToSpans GluteusMedius = LineSpansWithFrontOrBack Back []
muscleToSpans Quadriceps = LineSpansWithFrontOrBack Front []
muscleToSpans Core = LineSpansWithFrontOrBack Front []
muscleToSpans SideCore = LineSpansWithFrontOrBack Front []
muscleToSpans LowerBack = LineSpansWithFrontOrBack Back []
muscleToSpans UpperBack = LineSpansWithFrontOrBack Back []
muscleToSpans Calves = LineSpansWithFrontOrBack Back []
muscleToSpans Neck = LineSpansWithFrontOrBack Front [LineSpan 6 11 12]
muscleToSpans Delta = LineSpansWithFrontOrBack Front []
muscleToSpans Triceps = LineSpansWithFrontOrBack Back [LineSpan 8 4 4, LineSpan 9 3 3, LineSpan 10 2 2, LineSpan 8 18 18, LineSpan 9 19 19, LineSpan 10 20 20]
muscleToSpans HipFlexor = LineSpansWithFrontOrBack Front []
muscleToSpans Pecs = LineSpansWithFrontOrBack Front []
muscleToSpans Rotators = LineSpansWithFrontOrBack Front []
muscleToSpans BackQuadriceps = LineSpansWithFrontOrBack Back []
muscleToSpans Hamstrings = LineSpansWithFrontOrBack Back []

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
        applyToIdx (row - 1) (markupSet (colBegin, colEnd - colBegin + 1) trainingStateMarkup)
      -- 1. unpack line spans for a single muscle
      -- 2. fold spans with image, fixing markup due to muscle state
      transducer :: MuscleWithTrainingState -> [Markup a] -> [Markup a]
      transducer (MuscleWithTrainingState muscle' trainingState') image =
        let (LineSpansWithFrontOrBack muscleDirection spans) = muscleToSpans muscle'
         in if muscleDirection /= direction
              then image
              else foldr (spanTransducer (trainingStateToMarkup trainingState')) image spans
      lines' :: [Markup a]
      lines' = intersperse (fromText "\n") (foldr transducer markuppedImage muscles)
   in markup (mconcat lines')
