module Game where

import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Juicy
import GameTypes
import GameDraw
import GameInit
import GameHandle
import GameUpdate
-----------------------------
-- Main function for this app.
------------------------------

loadImages :: IO (Maybe Picture, Maybe Picture, Maybe Picture, Maybe Picture, Maybe Picture, Maybe Picture, Maybe Picture)
loadImages = do
  bgrd   <- loadJuicy "src/bg.png"
  pers   <- loadJuicy "src/pers.png"
  persCollis <- loadJuicy "src/persCollis.png"
  gover  <- loadJuicy "src/gover.png"
  money <- loadJuicy "src/money.png"
  bgRec   <- loadJuicy "src/bgRecords.png"
  compl <- loadJuicy "src/complexity.png"
  return (bgrd, pers, persCollis, gover, money, bgRec, compl)

imag :: Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Picture -> Images
imag bgrd pers persCollis gover money bgRec compl = Images{
  picBackground = bgrd, 
  picPlayer = pers,
  picPlayerCollis = persCollis, 
  picGameOver = gover,
  picMoney = money,
  picBgRecords = bgRec,
  picCompl = compl}

loadRecords :: String -> Maybe Records
loadRecords str = justRecords(map toInt (lines str))

justRecords :: [Maybe Int] -> Maybe Records
justRecords [] = Just []
justRecords (x : xs) = case x of
  Nothing -> Nothing
  Just z ->  case justRecords xs of
    Nothing -> Nothing
    Just y -> Just (z : y)

toInt:: String -> Maybe Int
toInt str = justInt (reverse (map findNumber str))
  where
      findNumber :: Char -> Maybe Int
      findNumber s = lookup s numberMap
      numberMap = zip names numbers
      numbers = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
      names = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']

justInt:: [Maybe Int] -> Maybe Int
justInt [] = Nothing
justInt [a] = a
justInt (x : xs) = case x of
  Nothing -> Nothing
  Just z -> case justInt xs of
    Nothing -> Nothing
    Just y -> Just (z + y * 10)

display :: Display
display = InWindow "Game" (screenWidth, screenHeight) (0,0)
bgColor :: Color
bgColor = black
fps :: Int
fps = 60


-- Run game. This is the ONLY unpure function.
runGame :: Int -> Records -> Images -> IO ()
runGame money records images = do
  playIO display bgColor fps (startGame money records) 
    (drawGame images) handleEvent updateGame