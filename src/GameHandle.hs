module GameHandle where

import Graphics.Gloss.Interface.IO.Game
import GameTypes
import GameInit
import System.Random
import GameUpdate

handleEvent :: Event -> GameState -> IO GameState
handleEvent (EventKey (MouseButton LeftButton) Down _ (x, y)) state
      | and[((gameMode state) == GameOver),
        x > 50, x < 350,
        y < (-120), y > (-170)]  = do
          g <- newStdGen
          return (initGame (gameMoneyScore state) g 
              (updateRecords (gameRecords state) (gameScore state)) (gameDefaultOffset state))
      | and [((gameMode state) == Record)] = return state{ gameMode = GameOver}
      | and[((gameMode state) == GameOver),
        x < 50, x > (-50),
        y < (-150), y > (-250)]  =  return state{ gameMode = Record }
      | and[((gameMode state) == GameOver),
        x < 400, x > 250,
        y < 200, y > 40]  =  return state{ gameMode = Settings }
      | and[((gameMode state) == Settings),
        x > (-250), x < 250,
        y > 20, y < 80]  =  do
          g <- newStdGen
          return 
            (initGame (gameMoneyScore state) g 
              (updateRecords (gameRecords state) (gameScore state)) easyOffset)
      | and[((gameMode state) == Settings),
        x > (-250), x < 250,
        y > (-90), y < (-30)]  =  do
          g <- newStdGen
          return 
            (initGame (gameMoneyScore state) g 
              (updateRecords (gameRecords state) (gameScore state)) defaultOffset)
      | and[((gameMode state) == Settings),
        x > (-250), x < 250,
        y > (-200), y < (-140)]  =  do
          g <- newStdGen
          return 
            (initGame (gameMoneyScore state) g 
              (updateRecords (gameRecords state) (gameScore state)) hardOffset)
      | and [((gameMode state) == GameOver), 
        (gameMoneyScore state) >= moneyRevival,
        x < (-50), x > (-350),
        y < (-120), y > (-170)
        ] = return state {
          gameCollisPlayer = True, 
          gameMoneyScore = (gameMoneyScore state) - moneyRevival,
          gameMode = InGame,
          gameCollisSteps = 300}
      |otherwise = return state
handleEvent (EventKey (SpecialKey KeySpace) Down _ _) state
  | ((gameMode state) == GameOver) = do
    g <- newStdGen
    return (initGame (gameMoneyScore state) g 
          (updateRecords (gameRecords state) (gameScore state)) (gameDefaultOffset state))
  | otherwise = return state
handleEvent (EventKey (SpecialKey KeyUp) Down _ _) state = return (makePlayerVert speed state)
handleEvent (EventKey (SpecialKey KeyUp) Up _ _) state = return (makePlayerVert 0 state)
handleEvent (EventKey (SpecialKey KeyDown) Down _ _) state = return (makePlayerVert (-speed) state)
handleEvent (EventKey (SpecialKey KeyDown) Up _ _) state = return (makePlayerVert 0 state)
handleEvent (EventKey (SpecialKey KeyLeft) Down _ _) state = return (makePlayerHor (-speed) state)
handleEvent (EventKey (SpecialKey KeyLeft) Up _ _) state = return (makePlayerHor 0 state)
handleEvent (EventKey (SpecialKey KeyRight) Down _ _) state = return (makePlayerHor speed state)
handleEvent (EventKey (SpecialKey KeyRight) Up _ _) state = return (makePlayerHor 0 state)
handleEvent _ state = return state

makePlayerVert :: Float -> GameState -> GameState
makePlayerVert sp state = state {gamePlayer = make (gamePlayer state)}
  where
    make player = player { ySpeed = sp }

makePlayerHor :: Float -> GameState -> GameState
makePlayerHor sp state = state {gamePlayer = make (gamePlayer state)}
  where
    make player = player { xSpeed = sp }