module Snake exposing (..)


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


type alias Position = (Int, Int)


type alias Snake =
  { body: List Position
  , moving: Direction
  , growth: Int
  , alive: Bool
  }


newSnake : Position -> Snake
newSnake position =
  { body = [position]
  , moving = Right
  , growth = 4
  , alive = True
  }


kill snake = { snake | alive = False }


head snake =
  List.head snake.body
  |> Maybe.withDefault (0,0)


tail snake = tailOrEmpty snake.body


changeDir snake dir =
  if snake.moving == (opposing dir) then
    snake
  else
    { snake | moving = dir }


wrap value max = if value < 0 then max else if value > max then 0 else value


tailOrEmpty = Maybe.withDefault [] << List.tail


growth = 4
grow snake = { snake | growth = snake.growth + growth }


move snake max =
  let (x,y) = head snake
      dropLast = List.reverse >> tailOrEmpty >> List.reverse
      tailTransform x = if snake.growth <= 0 then (dropLast x) else x
      dir = snake.moving
      (maxX, maxY) = max

      nextHead =
        case dir of
        Up -> (x, wrap (y - 1) maxY)
        Down -> (x, wrap (y + 1) maxY)
        Left -> (wrap (x - 1) maxX, y)
        Right -> (wrap (x + 1) maxX, y)

      newBody = tailTransform <| nextHead :: snake.body
      newGrowth = if snake.growth <= 0 then 0 else snake.growth-1
  in
    { snake | body = newBody, growth = newGrowth }

