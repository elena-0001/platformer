module Main where

import Game
import GameTypes
main :: IO ()
main = do
    images <- loadImages
    str <- readFile recordsPath
    case images of
      (Just bgrd, Just pers, Just persCollis, Just gover, Just money, Just bgRec, Just compl) ->  
        case loadRecords str of
          Nothing -> putStrLn "Wrong records"
          Just [] -> 
            runGame 0 rRecords (imag bgrd pers persCollis gover money bgRec compl) 
          Just [record] -> 
            runGame (record) rRecords (imag bgrd pers persCollis gover money bgRec compl) 
          Just records -> 
            runGame (head records) (tail records) (imag bgrd pers persCollis gover money bgRec compl) 
      _ -> putStrLn "Pictures error"
