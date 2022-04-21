module GameUpdate where

import GameTypes

updateGame :: Time -> GameState -> GameState
updateGame dt state
  | collision (gamePlayer state) (gameObstacles state) = state {gameMode = (False, False, True)}
  | otherwise = state{
      gamePlayer = updatePlayer dt (gamePlayer state),
      gameScore = (gameScore state) + dt,
      gameObstacles = updateObstacles dt (gameObstacles state)
      }
 
updatePlayer :: Time -> Player -> Player
updatePlayer dt player = moveHorizontally dt (moveVertically dt player)
 
moveHorizontally :: Time -> Player -> Player
moveHorizontally dt player
  |(xLeft player) + dt * (xSpeed player) <= leftEdge = player
  |(xRight player) + dt * (xSpeed player) >= rightEdge = player
  | otherwise = player{
      xLeft = (xLeft player) + dt * (xSpeed player),
      xRight = (xRight player) + dt * (xSpeed player)
      }
 
moveVertically :: Time -> Player -> Player
moveVertically dt player
  |(yBottom player) + dt * (ySpeed player) <= bottomEdge = player
  |(yTop player) + dt * (ySpeed player) >= topEdge = player
  |otherwise = player{
      yBottom = (yBottom player) + dt * (ySpeed player),
      yTop = (yTop player) + dt * (ySpeed player)
      }
 
updateObstacles :: Time -> [Obstacle] ->[Obstacle]
updateObstacles _ [] = []
updateObstacles dt (ob:obs)
  |(xRight ob) < leftEdge = updateObstacles dt obs
  |otherwise = (dx ob) : updateObstacles dt obs
    where
        dx obstacle = obstacle {
            xLeft = (xLeft obstacle) + dt * (xSpeed obstacle),
            xRight = (xRight obstacle) + dt * (xSpeed obstacle)
            }
 
collision :: Player -> [Obstacle] -> Bool
collision _ [] = False
collision player obstacles = or (map (collis player) (takeWhile onScreen obstacles))
  where onScreen obstacle = (xLeft obstacle) < rightEdge

collis :: Player -> Obstacle -> Bool
collis player obstacle = and [(collisionHor player obstacle), (collisionVert player obstacle)]
collisionHor :: Player -> Obstacle -> Bool
collisionHor player obstacle
  |and [((xLeft obstacle)<=(xRight player)), ((xLeft obstacle)>=(xLeft player))] = True
  |and [((xRight obstacle)>=(xLeft player)), ((xRight obstacle)<=(xRight player))] = True
  |otherwise = False
collisionVert :: Player -> Obstacle -> Bool
collisionVert player obstacle
  |and [((yBottom player)<=(yTop obstacle)), ((yBottom player)>=(yBottom obstacle))] = True
  |and [((yTop player)>=(yBottom obstacle)), ((yTop player)<=(yTop obstacle))] = True
  |otherwise = False