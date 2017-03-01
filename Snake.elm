module Snake exposing (..)
import Position exposing (Position)


type Direction =
    Up
  | Down
  | Left
  | Right


opposing dir =
  case dir of
  Up -> Down
  Down -> Up
  Left -> Right
  Right -> Left


type alias Snake =
  { headPosition: Position
  , moving: Direction
  , body: List Direction
  , growth: Int
  , alive: Bool
  }


kill snake = { snake | alive = False }


changeDir snake dir =
  if snake.moving == (opposing dir) then
    snake
  else
    { snake | moving = dir }


wrap value max = if value < 0 then max else if value > max then 0 else value


growth = 4
grow snake = { snake | growth = snake.growth + growth }


relativePosition : Position -> Direction -> Position
relativePosition (x,y) dir =
  case dir of
    Up -> (x,y + 1)
    Down -> (x,y)
    Left -> (x,y)
    Right -> (x,y)

toPositions : Snake -> List Position
toPositions snake = snake.headPosition :: []


snake1 =
  { headPosition = (10,10)
  , moving = Left
  , body = [Right, Down]
  , growth = 0
  , alive = True
  }
