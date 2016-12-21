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
  { body: List Position,
    moving: Direction
  }


changeDir snake dir =
  if snake.moving == (opposing dir) then
    snake
  else
    { snake | moving = dir }


wrap value max = if value < 0 then max else if value > max then 0 else value


tailOrEmpty = Maybe.withDefault [] << List.tail


move snake max =
  let (x,y) = Maybe.withDefault (0,0) (List.head snake.body)
      dropLast = List.reverse >> tailOrEmpty >> List.reverse
      dir = snake.moving
      (maxX, maxY) = max

      nextHead =
        case dir of
        Up -> (x, wrap (y - 1) maxY)
        Down -> (x, wrap (y + 1) maxY)
        Left -> (wrap (x - 1) maxX, y)
        Right -> (wrap (x + 1) maxX, y)

      newBody = dropLast <| nextHead :: snake.body
  in
    { snake | body = newBody }

