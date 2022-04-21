module GameInit where

import Graphics.Gloss.Interface.Pure.Game
import GameTypes
import System.Random

initGameInit :: StdGen -> GameState -> GameState
initGameInit g newstate = GameState
  {gameObstacles = absoluteObstacles 300 newstate (initObstacles g),
   gamePlayer = initPlayer,
   gameScore  = 0,
   gameBackground = initBackground,
   gameMode = gameMode newstate, 
   gamedefaultOffset = gamedefaultOffset newstate
  }

--initGameInit :: StdGen -> GameState
--initGameInit g = GameState
--  {gameBackground = initBackground,
--   gameMode = (True, False, False)
--  }

-- Инициализация игрового поля
--initGame :: StdGen -> GameState
--initGame g = GameState
-- {gameObstacles = absoluteObstacles 300 state (initObstacles g),
--  gamePlayer = initPlayer,
--  gameScore  = 0,
--  gameBackground = initBackground,
--  gameGameOver = False,
--  speedPlat = 550,
--  gamedefaultOffset = 200
--}
 
-- Инициализирование начальное состояние игрока
initPlayer :: Player
initPlayer = Square
  { xLeft = -500,
    yBottom = 0,
    xRight = -350,
    yTop = 83,
    xSpeed = 0,
    ySpeed = 0
  }

--  Создание бесконечный списка препятствий /подправить
absoluteObstacles :: Float -> GameState -> [Obstacle] -> [Obstacle]
absoluteObstacles _ _ [] = []
absoluteObstacles x g (ob:obs) = ob{
  xLeft = x - platformWidth, 
  xRight = x
  } : absoluteObstacles (x + (gamedefaultOffset g)) g obs 

-- Инициализирование случайного бесконечного списка препятствий для игрового поля
initObstacles :: StdGen -> [Obstacle]
initObstacles g = map initObstacle (randomRs obstacleHeight g)

-- Инициализация одной платформы
initObstacle :: Float -> Obstacle
initObstacle h = Square
  { xLeft = 0,
    xRight =0,
    yBottom = h - platformHeight/2,
    yTop = h + platformHeight/2,
    xSpeed = -speedPlat,
    ySpeed = 0
  }


initBackground :: Background
initBackground = Square{
    xLeft = -(fromIntegral screenWidth / 2),
    yBottom = -(fromIntegral screenHeight / 2),
    xRight = (fromIntegral screenWidth / 2),
    yTop = (fromIntegral screenHeight / 2),
    xSpeed = 0,
    ySpeed = 0
  }

-- | Инициализировать конец игры.
initGameOver :: Point
initGameOver = (0, 0)

defaultState :: GameState
defaultState = GameState
 {gameObstacles = [],
  gamePlayer = initPlayer,
  gameScore  = 0,
  gameBackground = initBackground,
  gameMode = (True, False, False),
--  gameGameOver = False,
--  speedPlat = 550,
  gamedefaultOffset = 200
 }