module GameUpdate where

import GameTypes

updateGame :: Time -> GameState -> IO GameState
updateGame dt state
  | and[
    (gameCollisPlayer state), 
    (gameCollisSteps state) <= 0,
    (gameMode state) == InGame
    ] = return state{
      gamePlayer = updatePlayer dt (gamePlayer state),
      gameScore = (gameScore state) + (round (1000*dt)),
      gameObstacles = updateObstacles dt (gameObstacles state),
      gameMoney = updateObstacles dt (gameMoney state),
      gameCollisPlayer = False
  }
  | and[ 
    collision (gamePlayer state) (gameObstacles state),
    not(gameCollisPlayer state),
    (gameMode state) == InGame
   ] =  do
          writeFile recordsPath (writeToFile ((gameMoneyScore state) : (gameRecords state)))
          return state{gameMode = GameOver}
  | and[
    collision (gamePlayer state) (gameMoney state),
    (gameMode state) == InGame
    ] = return state{
      gamePlayer = updatePlayer dt (gamePlayer state),
      gameScore = (gameScore state) + (round (1000*dt)),
      gameObstacles = updateObstacles dt (gameObstacles state),
      gameMoney = updateMoney dt (gamePlayer state) (gameMoney state),
      gameMoneyScore = (gameMoneyScore state) + 1,
      gameCollisSteps = ((gameCollisSteps state) - 1)
      }
  | ((gameMode state) == InGame) = return state{
      gamePlayer = updatePlayer dt (gamePlayer state),
      gameScore = (gameScore state) + (round (1000*dt)),
      gameObstacles = updateObstacles dt (gameObstacles state),
      gameMoney = updateObstacles dt (gameMoney state),
      gameCollisSteps = ((gameCollisSteps state) - 1)
      }
  | otherwise = return state

writeToFile :: Records -> String
writeToFile [] = ""
writeToFile (re : rex) = (show re)++"\n"++(writeToFile rex)

updateRecords :: Records -> Int -> Records
updateRecords [] _ = []
updateRecords (re : rex) score
  | (score <= re) = re : updateRecords rex score
  | otherwise = score : updateRecords rex re


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

updateMoney :: Time -> Player -> [Square] ->[Square]
updateMoney _ _ [] = []
updateMoney dt pl (ob:obs)
  |collis pl ob = updateObstacles dt obs
  |(xRight ob) < leftEdge = updateObstacles dt obs
  |otherwise = (dx ob) : updateObstacles dt obs
    where
        dx obstacle = obstacle {
            xLeft = (xLeft obstacle) + dt * (xSpeed obstacle),
            xRight = (xRight obstacle) + dt * (xSpeed obstacle)
            }

updateObstacles :: Time -> [Square] ->[Square]
updateObstacles _ [] = []
updateObstacles dt (ob:obs)
  |(xRight ob) < leftEdge = updateObstacles dt obs
  |otherwise = (dx ob) : updateObstacles dt obs
    where
        dx obstacle = obstacle {
            xLeft = (xLeft obstacle) + dt * (xSpeed obstacle),
            xRight = (xRight obstacle) + dt * (xSpeed obstacle)
            }
 
collision :: Player -> [Square] -> Bool
collision _ [] = False
collision player obstacles = or (map (collis player) (takeWhile onScreen obstacles))
  where onScreen obstacle = (xLeft obstacle) < rightEdge

collis :: Player -> Square -> Bool
collis player obstacle = and [(collisionHor player obstacle), (collisionVert player obstacle)]
collisionHor :: Player -> Square -> Bool
collisionHor player obstacle
  |and [((xLeft obstacle)<=(xRight player)), ((xLeft obstacle)>=(xLeft player))] = True
  |and [((xRight obstacle)>=(xLeft player)), ((xRight obstacle)<=(xRight player))] = True
  |otherwise = False
collisionVert :: Player -> Square -> Bool
collisionVert player obstacle
  |and [((yBottom player)<=(yTop obstacle)), ((yBottom player)>=(yBottom obstacle))] = True
  |and [((yTop player)>=(yBottom obstacle)), ((yTop player)<=(yTop obstacle))] = True
  |and [((yBottom obstacle)<=(yTop player)), ((yBottom obstacle)>=(yBottom player))] = True
  |and [((yTop obstacle)>=(yBottom player)), ((yTop obstacle)<=(yTop player))] = True
  |otherwise = False