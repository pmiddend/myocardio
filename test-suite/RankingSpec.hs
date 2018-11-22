module RankingSpec
  ( spec
  )
where

import Data.List(sort)
import           Test.Hspec                     ( describe
                                                , Spec
                                                )
import           Test.Hspec.QuickCheck          ( prop )
import           Test.QuickCheck                ( applyFun2
                                                , Fun
                                                )
import qualified Data.List.NonEmpty            as NE
import           Myocardio.Ranking              ( generateComplements )
import           Test.QuickCheck.Instances.Semigroup()

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
spec = describe "generate complements" $ do
  prop "should never change the number of list elements" lengthSame
  prop "should only shuffle elements, not delete" elementsSame
  prop "taking the first element is the identity"
       (\xs -> generateComplements (xs :: [Int]) (\_ _ -> 0) == xs)
