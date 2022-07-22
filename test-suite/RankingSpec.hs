{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module RankingSpec
  ( spec,
  )
where

import Control.Applicative ((<*>))
import Data.Bool
  ( Bool (True, False),
    (&&),
  )
import Data.Eq ((==))
import Data.Foldable
  ( maximum,
    minimum,
  )
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Int (Int)
import Data.List
  ( length,
    sort,
  )
import qualified Data.List.NonEmpty as NE
import Data.Maybe (Maybe (Just, Nothing))
import Data.Ord
  ( Ord,
    max,
    min,
    (<=),
    (>=),
  )
import Data.String (String)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock
  ( UTCTime (UTCTime),
    addUTCTime,
  )
import Myocardio.Exercise (Exercise (Exercise), name, muscles, tagged, last, category, reps)
import Myocardio.Muscle (Muscle (GluteusMaximus, Hamstrings, Pecs))
import Myocardio.Ranking
  ( RankedExercise (RankedExercise),
    exerciseComplement,
    generateComplements,
    rankTime,
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
    Fun,
    applyFun2,
    arbitraryBoundedEnum,
  )
import Test.QuickCheck.Arbitrary
  ( Arbitrary,
    arbitrary,
  )
import Test.QuickCheck.Instances.Semigroup
  (
  )
import Test.QuickCheck.Instances.Text ()
import Test.QuickCheck.Instances.Time ()
import Prelude
  ( Char,
    realToFrac,
    (-),
  )

instance Arbitrary Muscle where
  arbitrary = arbitraryBoundedEnum

clamp :: Ord a => a -> a -> a -> a
clamp minV maxV v = min maxV (max minV v)

lengthSame :: String -> Fun (Char, NE.NonEmpty Char) Int -> Bool
lengthSame as f =
  length
    ( generateComplements
        as
        (\x xs -> clamp 0 (NE.length xs - 1) (applyFun2 f x xs))
    )
    == length as

elementsSame :: String -> Fun (Char, NE.NonEmpty Char) Int -> Bool
elementsSame as f =
  sort
    ( generateComplements
        as
        (\x xs -> clamp 0 (NE.length xs - 1) (applyFun2 f x xs))
    )
    == sort as

defaultExercise name_ muscles_ last_ =
  Exercise
    { name = name_,
      muscles = muscles_,
      reps = "",
      category = "",
      last = last_,
      tagged = Nothing
    }

instance Arbitrary Exercise where
  arbitrary = Exercise <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

spec :: Spec
spec = do
  describe "generate complements" $ do
    prop "should never change the number of list elements" lengthSame
    prop "should only shuffle elements, not delete" elementsSame
    prop
      "taking the first element is the identity"
      (\xs -> generateComplements (xs :: [Int]) (\_ _ -> 0) == xs)

  describe "exercise complement" $ do
    it "should favor small intersections" $ do
      let referenceExercise =
            RankedExercise (Exercise {name = "1", muscles = [Pecs, GluteusMaximus]}) 3
          restExercises =
            [ RankedExercise (Exercise {name = "2", muscles = [Hamstrings]}) 3,
              RankedExercise (Exercise {name = "3", muscles = [GluteusMaximus, Hamstrings]}) 3
            ]
      exerciseComplement referenceExercise restExercises `shouldBe` 0

  describe "rank time" $ do
    it "should favor older exercises" $ do
      let oldDate = UTCTime (fromGregorian 2018 12 3) 0
          newDate = realToFrac 60 `addUTCTime` oldDate
          oldExercise = defaultExercise "older" [Pecs] (Just oldDate)
          newExercise = defaultExercise "newer" [Pecs] (Just newDate)
      rankTime [newExercise, oldExercise] `shouldBe` [0, 100]

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
          middleDate = realToFrac 60 `addUTCTime` oldDate
          newDate = realToFrac 120 `addUTCTime` oldDate
          oldExercise = defaultExercise "older" [Pecs] (Just oldDate)
          middleExercise = defaultExercise "older" [Pecs] (Just middleDate)
          newExercise = defaultExercise "newer" [Pecs] (Just newDate)
      rankTime [oldExercise, middleExercise, newExercise]
        `shouldBe` [100, 50, 0]

  describe "reorder" $ do
    it "alternate between groups" $ do
      let oldDate = UTCTime (fromGregorian 2018 12 3) 0
          newDate = realToFrac 60 `addUTCTime` oldDate
          aEx = defaultExercise "older" [Pecs] (Just oldDate)
          bEx = defaultExercise "newer" [GluteusMaximus] (Just newDate)
          aEx2 = defaultExercise "older" [Pecs] (Just newDate)
      reorderExercises [aEx, aEx2, bEx] `shouldBe` [aEx, bEx, aEx2]
