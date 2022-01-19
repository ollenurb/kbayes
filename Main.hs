module Main where

import Data.ByteString as BS
import Dataset (trainTestSplit)
import Classifier (train, predict)
import Dataset.SMS

main :: IO ()
main = do
  loadRes <- loadSMS "res/SMS.data"
  case loadRes of
    Nothing -> Prelude.putStrLn "Error: Couldn't load dataset SMS"
    Just ds -> do
        let model = train ds
        Prelude.putStrLn "Insert the string to predict:"
        toPredict <- BS.getLine
        print $ predict model toPredict
