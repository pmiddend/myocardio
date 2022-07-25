{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module RankingSpec
  ( spec,
  )
where

import Control.Applicative ((<*>))
import Data.Bool
  ( Bool (True),
    (&&),
  )
import Data.Foldable
  ( maximum,
    minimum,
  )
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Maybe (Maybe (Just, Nothing))
import Data.Ord
  ( (<=),
    (>=),
  )
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock
  ( UTCTime (UTCTime),
    addUTCTime,
  )
import Myocardio.Exercise (Exercise (Exercise), name, muscles, tagged, last, reps)
import Myocardio.Muscle (Muscle (GluteusMaximus, Pecs))
import Myocardio.Ranking
  ( rankTime,
    reorderExercises,
  )
import Test.Hspec
  ( Spec,
    describe,
    it,
    shouldBe,
  )
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
  ( Arbitrary (arbitrary),
    arbitraryBoundedEnum,
  )
import Test.QuickCheck.Instances.Semigroup
  (
  )
import Test.QuickCheck.Instances.Text ()
import Test.QuickCheck.Instances.Time ()
import Prelude
  ( realToFrac, Num ((*)),
  )
import Data.Int (Int)

instance Arbitrary Muscle where
  arbitrary = arbitraryBoundedEnum

defaultExercise name_ muscles_ last_ =
  Exercise
    { name = name_,
      muscles = muscles_,
      reps = "",
      last = last_,
      tagged = Nothing
    }

instance Arbitrary Exercise where
  arbitrary = Exercise <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

spec :: Spec
spec = do
  describe "rank time" $ do
    it "should favor older exercises" $ do
      let oldDate = UTCTime (fromGregorian 2018 12 3) 0
          newDate = realToFrac (86400 :: Int) `addUTCTime` oldDate
          oldExercise = defaultExercise "older" [Pecs] (Just oldDate)
          newExercise = defaultExercise "newer" [Pecs] (Just newDate)
      rankTime [newExercise, oldExercise] `shouldBe` [0, 100]

    it "should work on day basis" $ do
      let oldDate = UTCTime (fromGregorian 2018 12 3) 0
          newDate = realToFrac (60 :: Int) `addUTCTime` oldDate
          oldExercise = defaultExercise "older" [Pecs] (Just oldDate)
          newExercise = defaultExercise "newer" [Pecs] (Just newDate)
      rankTime [newExercise, oldExercise] `shouldBe` [0, 0]

    it "should favor exercises w/o date" $ do
      let oldDate = UTCTime (fromGregorian 2018 12 3) 0
          oldExercise = defaultExercise "older" [Pecs] (Just oldDate)
          noDateExercise = defaultExercise "newer" [Pecs] Nothing
      rankTime [oldExercise, noDateExercise] `shouldBe` [0, 0]

    prop "should not have values outside [0,100]" $ \exs -> do
      case rankTime exs of
        [] -> True
        xs -> minimum xs >= 0 && maximum xs <= 100

    it "should lerp across time" $ do
      let oldDate = UTCTime (fromGregorian 2018 12 3) 0
          middleDate = realToFrac (86400 :: Int) `addUTCTime` oldDate
          newDate = realToFrac (2 * 86400 :: Int) `addUTCTime` oldDate
          oldExercise = defaultExercise "older" [Pecs] (Just oldDate)
          middleExercise = defaultExercise "older" [Pecs] (Just middleDate)
          newExercise = defaultExercise "newer" [Pecs] (Just newDate)
      rankTime [oldExercise, middleExercise, newExercise]
        `shouldBe` [100, 50, 0]

  describe "reorder" $ do
    it "alternate between groups" $ do
      let oldDate = UTCTime (fromGregorian 2018 12 3) 0
          newDate = realToFrac (60 :: Int) `addUTCTime` oldDate
          aEx = defaultExercise "older" [Pecs] (Just oldDate)
          bEx = defaultExercise "newer" [GluteusMaximus] (Just newDate)
          aEx2 = defaultExercise "older" [Pecs] (Just newDate)
      reorderExercises [aEx, aEx2, bEx] `shouldBe` [aEx, bEx, aEx2]
