module Myocardio.ExerciseId where

import           Data.List                      ( zip
                                                , nub
                                                , filter
                                                , length
                                                , head
                                                )
import           Data.Function                  ( (.)
                                                , on
                                                )
import           Prelude                        ( )
import qualified Data.Text as T
import           Data.Text                      ( Text, unpack
                                                , take
                                                )
import           Text.Show                      ( Show(..) )
import           Data.Ord                       ( Ord )
import           Myocardio.Exercise             ( Exercise(name) )
import           Data.Functor                   ( (<$>) )
import           Data.Text.Encoding             ( encodeUtf8
                                                , decodeUtf8
                                                )
import           Data.ByteString.Base16         ( encode )
import           Crypto.Hash.SHA1               ( hash )
import           Myocardio.Endo                 ( Endo )
import           Data.Bifunctor                 ( first )
import           Data.Tuple                     ( fst )
import           Data.Eq                        ( Eq
                                                , (==)
                                                )
import Data.Int(Int)
import Data.Bool(Bool)

newtype ExerciseId = ExerciseId {
  getExId :: Text
  } deriving(Eq, Ord)

instance Show ExerciseId where
  show = unpack . getExId

exerciseIdLength :: ExerciseId -> Int
exerciseIdLength = T.length . getExId

takeId :: Int -> Endo ExerciseId
takeId n e = ExerciseId (take n (getExId e))

listUnique :: Eq a => [a] -> Bool
listUnique a = ((==) `on` length) a (nub a)

calculateIds :: [Exercise] -> [ExerciseId]
calculateIds exs =
  let hashSingle :: Exercise -> ExerciseId
      hashSingle = ExerciseId . decodeUtf8 . encode . hash . encodeUtf8 . name
      hashes :: [(ExerciseId, Exercise)]
      hashes = zip (hashSingle <$> exs) exs
      cutHashes :: Int -> Endo [(ExerciseId, Exercise)]
      cutHashes i = (first (takeId i) <$>)
      hashesUnique :: [(ExerciseId, Exercise)] -> Bool
      hashesUnique = listUnique . (fst <$>)
  in  fst <$> head (filter hashesUnique ((`cutHashes` hashes) <$> [1 ..]))
