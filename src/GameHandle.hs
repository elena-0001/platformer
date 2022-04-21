module GameHandle where

import Graphics.Gloss.Interface.Pure.Game
import GameTypes
import GameInit
import System.Random

handleEvent :: Event -> GameState -> GameState
handleEvent (EventKey (SpecialKey KeySpace) Down _ _) state
      | last_in_tuple(gameMode state)  = initGameInit (mkStdGen 100) state
      | otherwise = state
handleEvent (EventKey (SpecialKey KeyUp) Down _ _) state = makePlayerVert speed state
handleEvent (EventKey (SpecialKey KeyUp) Up _ _) state = makePlayerVert 0 state
handleEvent (EventKey (SpecialKey KeyDown) Down _ _) state = makePlayerVert (-speed) state
handleEvent (EventKey (SpecialKey KeyDown) Up _ _) state = makePlayerVert 0 state
handleEvent (EventKey (SpecialKey KeyLeft) Down _ _) state = makePlayerHor (-speed) state
handleEvent (EventKey (SpecialKey KeyLeft) Up _ _) state = makePlayerHor 0 state
handleEvent (EventKey (SpecialKey KeyRight) Down _ _) state = makePlayerHor speed state
handleEvent (EventKey (SpecialKey KeyRight) Up _ _) state = makePlayerHor 0 state
handleEvent (EventKey (Char 'h') Down _ _) state = gamemode 200 state                                                    
handleEvent (EventKey (Char 'l') Down _ _) state = gamemode 600 state
handleEvent (EventKey (SpecialKey KeyEnter) Down _ _) state = gamestart False state
handleEvent _ state = state
 

makePlayerVert :: Float -> GameState -> GameState
makePlayerVert sp state = state {gamePlayer = make (gamePlayer state)}
  where
    make player = player { ySpeed = sp }

makePlayerHor :: Float -> GameState -> GameState
makePlayerHor sp state = state {gamePlayer = make (gamePlayer state)}
  where
    make player = player { xSpeed = sp }
    
gamemode :: Float -> GameState -> GameState
gamemode offset state = state {gamedefaultOffset = offset}

gamestart :: Bool -> GameState -> GameState
gamestart False state = state {gameMode = (False, True, False)}
gamestart _ state = state

last_in_tuple :: (Bool, Bool, Bool) -> Bool
last_in_tuple (_, _, c) = c
