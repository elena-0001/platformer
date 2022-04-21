module GameDraw where

import Graphics.Gloss.Interface.Pure.Game
import GameTypes

drawGameInit :: Picture -> GameState -> Picture
-- drawGameInit bgrd_0 y z = drawBackground (picBackground pic) DrawGame y handleMouseClick
drawGameInit image state = pictures
   [drawBackground image, 
    drawText (h_or_l ++ (show $ gamedefaultOffset state))]
   

-- Выбор режима
drawText :: String -> Picture
drawText a = Color white $ scale 0.3 0.3 $ text (show a)


-- Отобразить игровое поле
--drawGame :: Images -> GameState-> Picture
--drawGame pic g = pictures
--  [drawGameInit (picBackground pic) (first_in_tuple(gameMode g)),
--   drawBackground (picBackground pic),
--   drawPlayer (picPlayer pic) (gamePlayer g),
--   drawObstacles (gameObstacles g),
--   drawScore (gameScore g),
--   drawGameOver (picGameOver pic) (last_in_tuple(gameMode g))
--  ]
  
drawGame :: Images -> GameState-> Picture
drawGame pic g = case (gameMode g) of 
  Settings -> drawGameInit (picBackground pic) g
  InGame -> pictures 
            [drawBackground (picBackground pic),
             drawPlayer (picPlayer pic) (gamePlayer g),
             drawObstacles (gameObstacles g),
             drawScore (gameScore g)]
  GameOver -> drawGameOver (picGameOver pic)

-- Нарисовать задний фон
drawBackground :: Picture -> Picture
drawBackground bg1 = translate x y bg1
    where
      (x, y) = (0, 0)

-- Нарисовать игрока
drawPlayer :: Picture -> Player -> Picture
drawPlayer image player = translate x y image
  where
    (x, y) = ((xLeft player) + 75, (yBottom player) + 41)


-- Написать счет
drawScore :: Float -> Picture
drawScore a = Color white (scale 0.2 0.2 (Translate x y (text (show a))))
 where
   (x, y) = (-2500, 1200)

-- Отобразить все препятствия игрового поля, помещающиеся на экран
drawObstacles :: [Obstacle] -> Picture
drawObstacles obs = pictures(map drawObstacle (takeWhile onScreen obs))
  where onScreen obstacle = (xLeft obstacle) < rightEdge


-- Нарисовать одно препятствие
drawObstacle :: Obstacle -> Picture 
drawObstacle ob = color (greyN 0.5) (translate x y (rectangleSolid (platformWidth) (platformHeight)))
 where
   (x, y) = ((xLeft ob) + platformWidth/2, (yBottom ob) + platformHeight/2)

drawGameOver :: Picture -> Picture
drawGameOver image = translate x y image
 where
   (x, y) = (0, 0)

last_in_tuple :: (Bool, Bool, Bool) -> Bool
last_in_tuple (_, _, c) = c

first_in_tuple :: (Bool, Bool, Bool) -> Bool
first_in_tuple (a, _, _) = a