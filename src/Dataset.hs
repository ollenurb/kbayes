module Dataset (FromLine (..),
                loadDataset,
                Dataset,
               ) where

import           Data.Attoparsec.ByteString (Parser, Result, feed, maybeResult, parse)
import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as BS
import           System.Random.Shuffle      (shuffle')
import           System.Random              (mkStdGen)

type Dataset a = [a]

class FromLine a where
    parseLine :: Parser a

loadDataset :: (FromLine a) => FilePath -> IO (Maybe (Dataset a))
loadDataset path = do
    content <- BS.readFile path
    {-- Split the file upon newlines, then filter empty strings --}
    let lines = filter (not . BS.null) . BS.split 10 $ content
    {-- Parse each line, then concatenate the result on the same Maybe --}
    let result = mapM (maybeResult . flip feed mempty . parse parseLine) lines
    return result

trainTestSplit :: Float -> Int -> Dataset a -> (Dataset a, Dataset a)
trainTestSplit sz seed dataset = splitAt testSz dataset
    where
        dsSz = length dataset
        shuffled = shuffle' dataset dsSz (mkStdGen seed)
        testSz = floor (sz * fromIntegral dsSz)

