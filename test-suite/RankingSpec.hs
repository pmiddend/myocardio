{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
module RankingSpec
  ( spec
  )
where

import           Data.List                      ( sort )
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
                                                )
import           Myocardio.Exercise             ( Exercise(..) )
import           Test.QuickCheck.Instances.Semigroup
                                                ( )

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
            [ RankedExercise (Exercise {name = "2", muscles = ["c"]}) 3
            , RankedExercise (Exercise {name = "3", muscles = ["b", "c"]}) 3
            ]
      exerciseComplement referenceExercise restExercises `shouldBe` 0
