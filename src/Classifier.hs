module Classifier where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Char (isLetter)
import Dataset (Dataset)

{- Here we first need some preprocessing functions  -}

sampleString :: ByteString
sampleString = B.pack "Free entry in 2 a wkly comp to win FA Cup final tkts 21st May 2005. Text FA to 87121 to receive entry question(std txt rate)T&C's apply 08452810075over18's"


{- We are going to consider only words with at least 3 characters -}
splitWords :: ByteString -> [ByteString]
splitWords = filter (\x -> B.length x > 3) . map (B.filter isLetter) . B.words
