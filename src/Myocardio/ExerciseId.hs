module Myocardio.ExerciseId where

import Crypto.Hash.SHA1 (hash)
import Data.Bifunctor (first)
import Data.Bool (Bool)
import Data.ByteString.Base16 (encode)
import Data.Eq
  ( Eq,
    (==),
  )
import Data.Function
  ( on,
    (.),
  )
import Data.Functor ((<$>))
import Data.Int (Int)
import Data.List
  ( filter,
    head,
    length,
    nub,
    zip,
  )
import Data.Ord (Ord)
import Data.Text
  ( Text,
    take,
    unpack,
  )
import qualified Data.Text as T
import Data.Text.Encoding
  ( decodeUtf8,
    encodeUtf8,
  )
import Data.Tuple (fst)
import Myocardio.Endo (Endo)
import Myocardio.Exercise (Exercise (name))
import Text.Show (Show (..))
import Prelude ()

newtype ExerciseId = ExerciseId
  { getExId :: Text
  }
  deriving (Eq, Ord)

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
   in fst <$> head (filter hashesUnique ((`cutHashes` hashes) <$> [1 ..]))
