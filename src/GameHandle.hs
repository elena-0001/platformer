module GameHandle where

import Graphics.Gloss.Interface.Pure.Game
import GameTypes
import GameInit
import System.Random

handleEvent :: Event -> GameState -> GameState
handleEvent (EventKey (SpecialKey KeySpace) Down _ _) state
      | (gameGameOver state)  = initGame (mkStdGen 100)
      | otherwise = state
handleEvent (EventKey (SpecialKey KeyUp) Down _ _) state = makePlayerVert speed state
handleEvent (EventKey (SpecialKey KeyUp) Up _ _) state = makePlayerVert 0 state
handleEvent (EventKey (SpecialKey KeyDown) Down _ _) state = makePlayerVert (-speed) state
handleEvent (EventKey (SpecialKey KeyDown) Up _ _) state = makePlayerVert 0 state
handleEvent (EventKey (SpecialKey KeyLeft) Down _ _) state = makePlayerHor (-speed) state
handleEvent (EventKey (SpecialKey KeyLeft) Up _ _) state = makePlayerHor 0 state
handleEvent (EventKey (SpecialKey KeyRight) Down _ _) state = makePlayerHor speed state
handleEvent (EventKey (SpecialKey KeyRight) Up _ _) state = makePlayerHor 0 state
handleEvent _ state = state
 

makePlayerVert :: Float -> GameState -> GameState
makePlayerVert sp state = state {gamePlayer = make (gamePlayer state)}
  where
    make player = player { ySpeed = sp }

makePlayerHor :: Float -> GameState -> GameState
makePlayerHor sp state = state {gamePlayer = make (gamePlayer state)}
  where
    make player = player { xSpeed = sp }