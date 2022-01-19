module Classifier where

import           Data.ByteString                  (ByteString)
import qualified Data.ByteString.Char8            as BS
import           Dataset.SMS (Target(..), SMS(..))
import           Data.Char                        (isLetter, toLower)
import qualified Data.HashMap.Strict              as HS
import           Data.Hashable (Hashable (..))
import qualified Data.HashMap.Strict as HS
import           Dataset (Dataset)

type Histogram a = HS.HashMap a Int
data Model = Model { pos :: Int
                   , neg :: Int
                   , posHist :: Histogram ByteString
                   , negHist :: Histogram ByteString
                   }

{- We are going to consider only words with at least 3 characters -}
splitWords :: ByteString -> [ByteString]
splitWords = filter ((> 3) . BS.length) . map (BS.map toLower . BS.filter isLetter) . BS.words

{- Compute cumulative counts per words -}
computeHistogram :: Target -> Dataset SMS -> Histogram ByteString
computeHistogram t = foldr (insertMany . splitWords . content) HS.empty -- Insert words into hs
                   . filter ((==t) . target)                            -- Filter by target
    where
        insertMany xs hs = foldr (\e a -> HS.insertWith (+) e 1 a) hs xs

{- Train the model -}
train :: Dataset SMS -> Model
train ds = Model pos neg posHist negHist
    where pos = HS.foldr (+) 0 posHist
          neg = HS.foldr (+) 0 negHist
          posHist = computeHistogram Ham ds
          negHist = computeHistogram Spam ds

{- Given a trained model and a string representing the message content, predict the target value -}
predict :: Model -> ByteString -> Target
predict (Model pos neg posHist negHist) sms
    | poslh > neglh = Ham
    | otherwise     = Spam
    where worded = splitWords sms
          likelihoods hist c = map (\w-> HS.lookupDefault 1 w hist `divide` c) worded
          total = pos + neg
          poslh = (pos `divide` total) * product (likelihoods posHist pos)
          neglh = (pos `divide` total) * product (likelihoods negHist neg)
          divide a b = fromIntegral a / fromIntegral b
