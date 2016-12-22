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


positionGen = Random.pair (Random.int 0 width) (Random.int 0 height)


nextFoodPosition seed = Random.step positionGen seed


moveSnake model =
  let newSnake = Snake.move model.snake boardIndex
      eating = Snake.head newSnake == model.food
      growingSnake = Snake.grow newSnake
      (nextFood, seed) = nextFoodPosition model.seed
  in
    if eating then
    { model | snake = growingSnake
    , food = nextFood
    , seed = seed
    }
    else
    { model | snake = newSnake}


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


boardSize = (80, 40)
width = Tuple.first boardSize
height = Tuple.second boardSize
boardIndex = (width-1, height-1)


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


init : (Model, Cmd Msg)
init = (Model ((Snake [(10,10)]) Right 4) (2,2) (Random.initialSeed 12345), Cmd.none)


main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }
