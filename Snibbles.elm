import Snake exposing (..)
import Board
import AsciiBoard

import Html exposing (Html)
import Html.Attributes as Att
import String
import Time exposing (Time)
import Keyboard
import Random


type alias Model =
  { snake: Snake
  , food: Snake.Position
  , seed: Random.Seed
  }


type Msg =
    Tick Time
  | Key Keyboard.KeyCode


positionGen = Random.pair (Random.int 0 (Tuple.first boardIndex)) (Random.int 0 (Tuple.second boardIndex))


obstacles model = model.snake.body


nextFoodPosition seed obstacles =
  let (randPos, nextSeed) = Random.step positionGen seed
      collided = collision randPos obstacles
  in
  if collided then
    nextFoodPosition nextSeed obstacles
  else
    (randPos, nextSeed)

collision = List.member


moveSnake model =
  let movedSnake = Snake.move model.snake boardIndex
      movedHead = Snake.head movedSnake
      eating = movedHead == model.food
      collided = collision movedHead (Snake.tail movedSnake)
      growingSnake = Snake.grow movedSnake
      modelMoved = { model | snake = movedSnake }
      (nextFood, seed) = nextFoodPosition model.seed (obstacles modelMoved)
  in
    if collided then
      { model | snake = Snake.kill model.snake }
    else if eating then
      { model
      | snake = growingSnake
      , food = nextFood
      , seed = seed
      }
    else
      modelMoved


onTick model =
  ( moveSnake model
  , Cmd.none
  )


changeDir model dir =
  ( { model | snake = Snake.changeDir model.snake dir}
  , Cmd.none
  )


onKey code model =
  case code of
    37 -> changeDir model Snake.Left
    39 -> changeDir model Snake.Right
    38 -> changeDir model Snake.Up
    40 -> changeDir model Snake.Down
    _ -> (model, Cmd.none)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick time -> onTick model
    Key code -> onKey code model


board : Model -> String
board model =
  Board.emptyBoard width height
  |> Board.addSnake model.snake.body
  |> Board.addFood model.food
  |> AsciiBoard.boardToString


view : Model -> Html Msg
view model =
  Html.div []
  [ Html.pre []
    [ Html.text (board model)
    ]
  ]


subscriptions : Model -> Sub Msg
subscriptions model = Sub.batch [Time.every (Time.second/8) Tick, Keyboard.presses Key]


boardSize = (20, 10)
width = Tuple.first boardSize
height = Tuple.second boardSize
boardIndex = (width-1, height-1)


init : (Model, Cmd Msg)
init = (Model ((Snake [(0,0)]) Right 4 True) (2,2) (Random.initialSeed 12345), Cmd.none)


main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }
