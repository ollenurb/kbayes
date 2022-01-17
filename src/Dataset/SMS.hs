{-# LANGUAGE OverloadedStrings #-}

module Dataset.SMS where

import           Control.Applicative              ((<|>))
import           Data.Attoparsec.ByteString       (Parser (..))
import           Data.Attoparsec.ByteString.Char8 (char, string, takeByteString)
import           Data.ByteString                  (ByteString)
import qualified Data.ByteString.Char8            as BS
import           Data.Char                        (isLetter, toLower)
import           Dataset                          (FromLine (..), loadDataset, Dataset)
import qualified Data.HashMap.Strict              as HS
import           Data.Hashable (Hashable (..))

loadSMS :: FilePath -> IO (Maybe (Dataset SMS))
loadSMS = loadDataset

data SMS = SMS { target  :: Target
               , content :: ByteString
               }
               deriving (Show, Eq)

data Target = Ham | Spam deriving (Show, Eq)

instance Hashable Target where
    hashWithSalt i = hashWithSalt i . show

instance FromLine SMS where
    parseLine = SMS <$>
        parseTarget <*>
        (char '\t' >> takeByteString)

parseTarget :: Parser Target
parseTarget = (Ham <$ string "ham") <|> (Spam <$ string "spam")

{-
    What I want to archieve here is that I need the list of all the possible
    features. To compute them, I actually need to process every single message,
    splitting it into words, then pushing such words into a set. This is going
    to be our vocabulary. While doing this, I'll need to transform the dataset
    into the list of words
 -}

type Histogram a = HS.HashMap a Int
data Model = Model { pos :: Int
                   , neg :: Int
                   , posHist :: Histogram ByteString
                   , negHist :: Histogram ByteString
                   }

{- We are going to consider only words with at least 3 characters -}
splitWords :: ByteString -> [ByteString]
splitWords = filter ((> 3) . BS.length) . map (BS.map toLower . BS.filter isLetter) . BS.words

insertMany :: Hashable a => [a] -> Histogram a -> Histogram a
insertMany xs hs = foldr (\e a -> HS.insertWith (+) e 1 a) hs xs

computeHistogram :: Target -> Dataset SMS -> Histogram ByteString
computeHistogram t = foldr (insertMany . splitWords . content) HS.empty -- Insert words into hs
                   . filter ((==t) . target)                            -- Filter by target

train :: Dataset SMS -> Model
train ds = Model pos neg posHist negHist
    where pos = HS.foldr (+) 0 posHist
          neg = HS.foldr (+) 0 negHist
          posHist = computeHistogram Ham ds
          negHist = computeHistogram Spam ds


predict :: Model -> ByteString -> Target
predict (Model pos neg posHist negHist) sms
    | poslh > neglh = Ham
    | otherwise     = Spam
    where worded = splitWords sms
          likelihoods hist c = map (\w-> HS.lookupDefault 1 w hist `divide` c) worded
          total = pos + neg
          poslh = (pos `divide` total) * product (likelihoods posHist pos)
          neglh = (pos `divide` total) * product (likelihoods negHist neg)

divide :: (Integral a, Fractional b) => a -> a -> b
divide a b = fromIntegral a / fromIntegral b
