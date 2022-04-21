module Game where

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Juicy
import GameTypes
import GameDraw
import GameInit
import GameHandle
import GameUpdate
import System.Random
-----------------------------
-- Main function for this app.
------------------------------

loadImages :: IO (Maybe Picture, Maybe Picture, Maybe Picture)
loadImages = do
  bgrd   <- loadJuicy "src/bg.png"
  pers   <- loadJuicy "src/pers.png"
  gover   <- loadJuicy "src/gameover.png"
  return (bgrd, pers, gover)

imag :: Picture -> Picture -> Picture -> Images
imag bgrd pers gover = Images{
  picBackground = bgrd,
  picPlayer = pers,
  picGameOver = gover} 


-- Run game. This is the ONLY unpure function.
runGame :: Images -> IO ()
runGame images= do
  g <- newStdGen
  play display bgColor fps (initGameInit g defaultState) (drawGame images) handleEvent updateGame
  where
    display = InWindow "Game" (screenWidth, screenHeight) (0,0)
    bgColor = black
    fps     = 60
