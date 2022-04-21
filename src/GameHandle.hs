module GameHandle where

import Graphics.Gloss.Interface.Pure.Game
import GameTypes
import GameInit
import System.Random

handleEvent :: Event -> GameState -> GameState
handleEvent (EventKey (SpecialKey KeySpace) Down _ _) state
      | GameOver <- (gameMode state)  = initGameInit (mkStdGen 100) state   
      | otherwise = state
handleEvent (EventKey (SpecialKey KeyUp) Down _ _) (GameState sa sb sc sd InGame se) = makePlayerVert speed (GameState sa sb sc sd InGame se)
handleEvent (EventKey (SpecialKey KeyUp) Up _ _) (GameState sa sb sc sd InGame se) = makePlayerVert 0 (GameState sa sb sc sd InGame se)
handleEvent (EventKey (SpecialKey KeyDown) Down _ _) (GameState sa sb sc sd InGame se) = makePlayerVert (-speed) (GameState sa sb sc sd InGame se)
handleEvent (EventKey (SpecialKey KeyDown) Up _ _) (GameState sa sb sc sd InGame se) = makePlayerVert 0 (GameState sa sb sc sd InGame se)
handleEvent (EventKey (SpecialKey KeyLeft) Down _ _) (GameState sa sb sc sd InGame se) = makePlayerHor (-speed) (GameState sa sb sc sd InGame se)
handleEvent (EventKey (SpecialKey KeyLeft) Up _ _) (GameState sa sb sc sd InGame se) = makePlayerHor 0 (GameState sa sb sc sd InGame se)
handleEvent (EventKey (SpecialKey KeyRight) Down _ _) (GameState sa sb sc sd InGame se) = makePlayerHor speed (GameState sa sb sc sd InGame se)
handleEvent (EventKey (SpecialKey KeyRight) Up _ _) (GameState sa sb sc sd InGame se) = makePlayerHor 0 (GameState sa sb sc sd InGame se)
handleEvent (EventKey (Char 'h') Down _ _) (GameState sa sb sc sd Settings _) = (GameState sa sb sc sd Settings 200)                                                   
handleEvent (EventKey (Char 'l') Down _ _) (GameState sa sb sc sd Settings _) = (GameState sa sb sc sd Settings 600)
handleEvent (EventKey (SpecialKey KeyEnter) Down _ _) (GameState sa sb sc sd Settings se) = gamestart False (GameState sa sb sc sd Settings se)
handleEvent _ state = state
 

makePlayerVert :: Float -> GameState -> GameState
makePlayerVert sp state = state {gamePlayer = make (gamePlayer state)}
  where
    make player = player { ySpeed = sp }

makePlayerHor :: Float -> GameState -> GameState
makePlayerHor sp state = state {gamePlayer = make (gamePlayer state)}
  where
    make player = player { xSpeed = sp }
    

gamestart :: Bool -> GameState -> GameState
gamestart False state = state {gameMode = InGame}
gamestart _ state = state

