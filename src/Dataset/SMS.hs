{-# LANGUAGE OverloadedStrings #-}

module Dataset.SMS where

import           Control.Applicative              ((<|>))
import           Data.Attoparsec.ByteString       (Parser (..))
import           Data.Attoparsec.ByteString.Char8 (char, string, takeByteString)
import           Data.ByteString                  (ByteString)
import qualified Data.ByteString                  as BS
import           Dataset                          (FromLine (..), loadDataset)

data SMS = SMS { target  :: Target
               , content :: ByteString
               }
               deriving (Show, Eq)

data Target = Ham | Spam deriving (Show, Eq)

instance FromLine SMS where
    parseLine = SMS <$>
        parseTarget <*>
        (char '\t' >> takeByteString)

parseTarget :: Parser Target
parseTarget = (Ham <$ string "ham") <|> (Spam <$ string "spam")
