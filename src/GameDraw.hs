module GameDraw where

import Graphics.Gloss.Interface.IO.Game
import GameTypes


-- Отобразить игровое поле
drawGame :: Images -> GameState-> IO Picture
drawGame pic g
  | ((gameMode g) == InGame) = return (pictures
    [
      drawBackground (picBackground pic),
      drawPlayer (picPlayer pic) (picPlayerCollis pic) (gameCollisPlayer g) (gamePlayer g),
      drawObstacles (gameObstacles g),
      drawMoney (picMoney pic) (gameMoney g),
      drawScore (gameScore g),
      drawMoneyScore (picMoney pic) (gameMoneyScore g)
    ])
  | ((gameMode g) == GameOver) = return (pictures
    [
      drawBackground (picBackground pic),
      drawPlayer (picPlayer pic) (picPlayerCollis pic) (gameCollisPlayer g) (gamePlayer g),
      drawObstacles (gameObstacles g),
      drawMoney (picMoney pic) (gameMoney g),
      drawScore (gameScore g),
      drawMoneyScore (picMoney pic) (gameMoneyScore g),
      drawGameOver (picGameOver pic)
    ])
  | ((gameMode g) == Record) = return (drawRecords (picBgRecords pic) (gameRecords g))
  | ((gameMode g) == Settings) = return (drawSettings (picCompl pic))
  | otherwise = do 
      putStrLn "Error Game Mod"
      return blank
-- Нарисовать задний фон
drawBackground :: Picture -> Picture
drawBackground bg1 = translate x y bg1
    where
      (x, y) = (0, 0)

-- Нарисовать игрока
drawPlayer :: Picture -> Picture -> Bool -> Player -> Picture
drawPlayer image1 image2 col player
  | col = translate x y image2
  | otherwise = translate x y image1
      where
        (x, y) = ((xLeft player) + 75, (yBottom player) + 41)


-- Написать счет
drawScore :: Int -> Picture
drawScore a = color white (scale 0.2 0.2 (translate x y (text (show a))))
 where
   (x, y) = (-2500, 1200)

drawMoneyScore :: Picture -> Int -> Picture
drawMoneyScore pic a = pictures[
  color white (scale 0.2 0.2 (translate x1 y1 (text (show a)))),
  translate x2 y2 (scale 0.7 0.7 pic)]
 where
   (x1, y1) = (2300, 1200)
   (x2, y2) = (400, 250)

-- Отобразить все препятствия игрового поля, помещающиеся на экран
drawObstacles :: [Obstacle] -> Picture
drawObstacles obs = pictures(map drawObstacle (takeWhile onScreen obs))
  where onScreen obstacle = (xLeft obstacle) < rightEdge
-- Нарисовать одно препятствие
drawObstacle :: Obstacle -> Picture 
drawObstacle ob = color (greyN 0.5) (translate x y (rectangleSolid (platformWidth) (platformHeight)))
 where
   (x, y) = ((xLeft ob) + platformWidth/2, (yBottom ob) + platformHeight/2)

drawMoney :: Picture -> [Money] -> Picture
drawMoney pic mon = pictures(map (drawOneMoney pic) (takeWhile onScreen mon))
  where onScreen money = (xLeft money) < rightEdge
drawOneMoney :: Picture -> Money -> Picture 
drawOneMoney pic mon = (translate x y pic)
 where
   (x, y) = ((xLeft mon) + moneyWidth/2, (yBottom mon) + moneyHeight/2)
drawGameOver :: Picture -> Picture
drawGameOver image  = translate x y image
 where
   (x, y) = (0, 0)

drawRecords :: Picture -> Records -> Picture
drawRecords bg records = pictures[translate 0 0 bg, pictures(map drawRecord (zip numb records))]

drawRecord :: (Int, Int) -> Picture
drawRecord (n, record) = pictures [draw n x1 y1, draw record x2 y2]
  where 
    draw a x y = color white (translate x y (scale 0.3 0.3 (text (show a))))
    (x1, y1) = (-200, 135 - (fromIntegral n) * 40)
    (x2, y2) = (0, 135 - (fromIntegral n) * 40)

drawSettings :: Picture -> Picture
drawSettings image = translate x y image
 where
   (x, y) = (0, 0)