{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
module RankingSpec
  ( spec
  )
where

import           Control.Applicative            ( (<*>) )
import           Data.Functor                   ( (<$>) )
import           Data.Foldable                  ( minimum
                                                , maximum
                                                )
import           Test.Hspec                     ( describe
                                                , Spec
                                                , it
                                                , shouldBe
                                                )
import           Test.Hspec.QuickCheck          ( prop )
import           Test.QuickCheck                ( applyFun2
                                                , Fun
                                                )
import qualified Data.List.NonEmpty            as NE
import           Myocardio.Ranking              ( generateComplements
                                                , RankedExercise(..)
                                                , exerciseComplement
                                                , rankTime
                                                , reorderExercises
                                                )
import           Myocardio.Exercise             ( Exercise(..) )
import           Test.QuickCheck.Instances.Semigroup
                                                ( )
import           Test.QuickCheck.Instances.Text ( )
import           Test.QuickCheck.Instances.Time ( )
import           Data.Time.Calendar             ( fromGregorian )
import           Data.Time.Clock                ( UTCTime(..)
                                                , addUTCTime
                                                )
import           Prelude                        ( realToFrac
                                                , (-)
                                                , Char
                                                )
import           Data.Int                       ( Int )
import           Data.Bool                      ( Bool(..)
                                                , (&&)
                                                )
import           Data.Function                  ( ($) )
import           Data.Ord                       ( Ord
                                                , min
                                                , max
                                                , (>=)
                                                , (<=)
                                                )
import           Data.String                    ( String )
import           Data.Maybe                     ( Maybe(..) )
import           Data.List                      ( length
                                                , sort
                                                )
import           Data.Eq                        ( (==) )
import           Test.QuickCheck.Arbitrary      ( Arbitrary
                                                , arbitrary
                                                )

clamp :: Ord a => a -> a -> a -> a
clamp minV maxV v = min maxV (max minV v)

lengthSame :: String -> Fun (Char, NE.NonEmpty Char) Int -> Bool
lengthSame as f =
  length
      (generateComplements
        as
        (\x xs -> clamp 0 (NE.length xs - 1) (applyFun2 f x xs))
      )
    == length as

elementsSame :: String -> Fun (Char, NE.NonEmpty Char) Int -> Bool
elementsSame as f =
  sort
      (generateComplements
        as
        (\x xs -> clamp 0 (NE.length xs - 1) (applyFun2 f x xs))
      )
    == sort as

defaultExercise name muscles last = Exercise name muscles "" "" last Nothing

instance Arbitrary Exercise where
  arbitrary = Exercise <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

spec :: Spec
spec = do
  describe "generate complements" $ do
    prop "should never change the number of list elements" lengthSame
    prop "should only shuffle elements, not delete"        elementsSame
    prop "taking the first element is the identity"
         (\xs -> generateComplements (xs :: [Int]) (\_ _ -> 0) == xs)

  describe "exercise complement" $ do
    it "should favor small intersections" $ do
      let referenceExercise =
            RankedExercise (Exercise {name = "1", muscles = ["a", "b"]}) 3
          restExercises =
            [ RankedExercise (Exercise {name = "2", muscles = ["c"]})      3
            , RankedExercise (Exercise {name = "3", muscles = ["b", "c"]}) 3
            ]
      exerciseComplement referenceExercise restExercises `shouldBe` 0

  describe "rank time" $ do
    it "should favor older exercises" $ do
      let oldDate     = UTCTime (fromGregorian 2018 12 3) 0
          newDate     = realToFrac 60 `addUTCTime` oldDate
          oldExercise = defaultExercise "older" ["a"] (Just oldDate)
          newExercise = defaultExercise "newer" ["a"] (Just newDate)
      rankTime [newExercise, oldExercise] `shouldBe` [0, 100]

    it "should favor exercises w/o date" $ do
      let oldDate        = UTCTime (fromGregorian 2018 12 3) 0
          oldExercise    = defaultExercise "older" ["a"] (Just oldDate)
          noDateExercise = defaultExercise "newer" ["a"] Nothing
      rankTime [oldExercise, noDateExercise] `shouldBe` [0, 100]

    prop "should not have values outside [0,100]" $ \exs -> do
      case rankTime exs of
        [] -> True
        xs -> minimum xs >= 0 && maximum xs <= 100

    it "should lerp across time" $ do
      let oldDate        = UTCTime (fromGregorian 2018 12 3) 0
          middleDate     = realToFrac 60 `addUTCTime` oldDate
          newDate        = realToFrac 120 `addUTCTime` oldDate
          oldExercise    = defaultExercise "older" ["a"] (Just oldDate)
          middleExercise = defaultExercise "older" ["a"] (Just middleDate)
          newExercise    = defaultExercise "newer" ["a"] (Just newDate)
      rankTime [oldExercise, middleExercise, newExercise]
        `shouldBe` [100, 50, 0]

  describe "reorder" $ do
    it "alternate between groups" $ do
      let oldDate = UTCTime (fromGregorian 2018 12 3) 0
          newDate = realToFrac 60 `addUTCTime` oldDate
          aEx     = defaultExercise "older" ["a"] (Just oldDate)
          bEx     = defaultExercise "newer" ["b"] (Just newDate)
          aEx2    = defaultExercise "older" ["a"] (Just newDate)
      reorderExercises [aEx, aEx2, bEx] `shouldBe` [aEx, bEx, aEx2]
