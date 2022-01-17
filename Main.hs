module Main where

import Data.ByteString as BS
import Dataset (trainTestSplit)
import Dataset.SMS (loadSMS, splitWords, computeHistogram, Target(..), SMS(..))
import Dataset.SMS as M

main :: IO ()
main = do
  loadRes <- loadSMS "res/SMS.data"
  case loadRes of
    Nothing -> Prelude.putStrLn "Error: Couldn't load dataset SMS"
    Just ds -> do
        let model = M.train ds
        Prelude.putStrLn "Insert the string to predict:"
        toPredict <- BS.getLine
        print $ M.predict model toPredict


-- main :: IO ()
-- main = do
--     loadRes <- loadSMS "res/SMS.data"
--     case loadRes of
--       Nothing -> putStrLn "Error: Couldn't load dataset SMS"
--       Just ds -> do
--           let seed = 42
--           let (train, test) = trainTestSplit 0.1 seed ds
--           let res = map (splitWords . content) train
--           print  res
