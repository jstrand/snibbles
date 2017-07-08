module Snake exposing (..)


type Direction =
    Up
  | Down
  | Left
  | Right


opposing : Direction -> Direction
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
  , respawnIn: Int
  }


newSnakeWithDir : Position -> Direction -> Snake
newSnakeWithDir position direction =
  { body = [position]
  , moving = direction
  , growth = 4
  , respawnIn = 0
  }


newSnake : Position -> Snake
newSnake position = newSnakeWithDir position Right


kill : Snake -> Snake
kill snake = { snake | respawnIn = 10 }


isAlive : Snake -> Bool
isAlive snake = snake.respawnIn <= 0


isDead : Snake -> Bool
isDead = not << isAlive


recouperate : Snake -> Snake
recouperate snake =
  { snake
  | respawnIn = if snake.respawnIn == 0 then 0 else snake.respawnIn - 1
  }


isRespawning : Snake -> Bool
isRespawning snake = snake.respawnIn == 1


head : Snake -> Position
head snake =
  List.head snake.body
  |> Maybe.withDefault (0,0)


tail : Snake -> List Position
tail snake = tailOrEmpty snake.body


body : Snake -> List Position
body = .body


changeDir : Direction -> Snake -> Snake
changeDir dir snake =
  if snake.moving == (opposing dir) then
    snake
  else
    { snake | moving = dir }


wrap : Int -> Int -> Int
wrap value max = if value < 0 then max else if value > max then 0 else value


tailOrEmpty : List a -> List a
tailOrEmpty = Maybe.withDefault [] << List.tail


growth : Int
growth = 4


grow : Snake -> Snake
grow snake = { snake | growth = snake.growth + growth }


move : Snake -> Position -> Snake
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
      newGrowth = if snake.growth <= 0 then 0 else snake.growth - 1
  in
    { snake
    | body = newBody
    , growth = newGrowth
    }

