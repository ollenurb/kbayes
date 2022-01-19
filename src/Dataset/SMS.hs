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
