module Game exposing (..)

import Random

import Snake exposing (..)
import Dict exposing (Dict)


boardSize = (30, 10)
width = Tuple.first boardSize
height = Tuple.second boardSize
boardIndex = (width-1, height-1)


type alias SnakeId = Int


type alias Game =
  { snakes: Dict SnakeId Snake
  , food: Snake.Position
  , seed: Random.Seed
  , ticks: Int
  , error: String
  }


emptyGame : Game
emptyGame =
  { snakes = Dict.empty
  , food = (5,5)
  , seed = Random.initialSeed 12345
  , ticks = 0
  , error = ""
  }


newGame : Game
newGame =
     emptyGame
  |> placeFood


addSnake : SnakeId -> Direction -> Game -> Game
addSnake snakeId dir game =
  let
    (position, nextSeed) = randomGamePosition game
    newSnake = Snake.newSnakeWithDir position dir
  in
    { game
    | snakes = Dict.insert snakeId newSnake game.snakes
    , seed = nextSeed
    }

updateSnake : SnakeId -> Snake -> Game -> Game
updateSnake snakeId snake game =
  { game
  | snakes = Dict.insert snakeId snake game.snakes
  }

positionGen = Random.pair (Random.int 0 (width-1)) (Random.int 0 (height-1))

snakePartPositions = obstacles

obstacles : Game -> List Position
obstacles model =
  let
    getBody _ snake = snake.body
  in
    model.snakes
    |> Dict.map getBody
    |> Dict.values
    |> List.concat


placeFood game =
  let
    (nextFood, nextSeed) = randomGamePosition game
  in
    { game
    | food = nextFood
    , seed = nextSeed
    }


randomGamePosition game = randomEmptyPosition game.seed (obstacles game)


randomEmptyPosition seed obstacles =
  let (randPos, nextSeed) = Random.step positionGen seed
      collided = detectCollision randPos obstacles
  in
  if collided then
    randomEmptyPosition nextSeed obstacles
  else
    (randPos, nextSeed)


detectCollision = List.member


