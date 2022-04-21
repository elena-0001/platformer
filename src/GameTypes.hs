module GameTypes where

import Graphics.Gloss.Interface.Pure.Game

type Time = Float

data Square = Square{
    xLeft :: Float
    ,xRight :: Float
    ,yBottom :: Float
    ,yTop :: Float
    ,xSpeed :: Float
    ,ySpeed :: Float
    }
type Player = Square
type Obstacle = Square
type Background = Square
    
data GameState = GameState{
    gameObstacles :: [Obstacle]
    ,gamePlayer :: Player   
    ,gameBackground :: Background
    ,gameScore :: Float
--    ,gameGameOver :: Bool
--    ,gameScreen :: Bool -- new
    ,gameMode :: (Bool, Bool, Bool) --gameScreen gameGame gameGameOver 
--    ,speedPlat :: Float --new
    ,gamedefaultOffset :: Float --new
    }

data Images = Images{
    --picObstacle :: Picture
    picPlayer :: Picture
    ,picBackground :: Picture
    ,picGameOver :: Picture
    }



speed :: Float
speed = 500


-- delete, User will enter
speedPlat :: Float
speedPlat = 550

numElement :: Int
numElement = 10

leftEdge :: Float
leftEdge = - (fromIntegral screenWidth)/2
rightEdge :: Float
rightEdge = (fromIntegral screenWidth)/2
bottomEdge :: Float
bottomEdge = - (fromIntegral screenHeight)/2
topEdge :: Float
topEdge = (fromIntegral screenHeight)/2

screenWidth :: Int
screenWidth = 1070

screenHeight :: Int
screenHeight = 550

platformWidth :: Float
platformWidth = 20
platformHeight :: Float
platformHeight = 100

-- delete, user will enter
defaultOffset :: Float
defaultOffset = 200

obstacleHeight :: (Float, Float)
obstacleHeight = (-w, w)
  where
    w = (fromIntegral screenHeight - platformHeight) / 2
    
h_or_l :: String
h_or_l = "Enter h or l" 

