module Main where

import Game
main :: IO ()
main = do
    images <- loadImages
    case images of
        (Just bgrd, Just pers, Just gover) -> runGame (imag bgrd pers gover)
        _ -> putStrLn "Pictures error"
