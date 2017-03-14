import Snake exposing (..)
import Board
import AsciiBoard

import Html exposing (Html)
import Html.Attributes as Att
import Time exposing (Time)
import Keyboard
import Random
import WebSocket


type alias Model =
  { snake: Snake
  , food: Snake.Position
  , seed: Random.Seed
  , ticks: Int
  }


type Msg =
    Tick Time
  | Key Keyboard.KeyCode
  | IncomingMessage String


server = "ws://localhost:9000"


positionGen = Random.pair (Random.int 0 (width-1)) (Random.int 0 (height-1))


obstacles model = model.snake.body


nextFoodPosition seed obstacles =
  let (randPos, nextSeed) = Random.step positionGen seed
      collided = detectCollision randPos obstacles
  in
  if collided then
    nextFoodPosition nextSeed obstacles
  else
    (randPos, nextSeed)


detectCollision = List.member


moveSnake model =
  let movedSnake = Snake.move model.snake boardIndex
      movedHead = Snake.head movedSnake
      eating = movedHead == model.food
      collided = detectCollision movedHead (Snake.tail movedSnake)
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
  let movedModel = moveSnake model
  in
    { movedModel | ticks = model.ticks + 1 }


changeDir dir model =
  { model | snake = Snake.changeDir model.snake dir}


dirAsString dir =
  case dir of
    Snake.Left -> "L"
    Snake.Right -> "R"
    Snake.Up -> "U"
    Snake.Down -> "D"


stringAsDir str =
  case str of
    "L" -> Snake.Left
    "R" -> Snake.Right
    "U" -> Snake.Up
    "D" -> Snake.Down
    _ -> Snake.Right


handleIncomingMessage message =
  case message of
    "T" -> onTick
    _ -> onTick << changeDir (stringAsDir message)


sendDir dir = WebSocket.send server (dirAsString dir)


onKey code model =
  case code of
    37 -> (model, sendDir Snake.Left)
    39 -> (model, sendDir Snake.Right)
    38 -> (model, sendDir Snake.Up)
    40 -> (model, sendDir Snake.Down)
    _ -> (model, Cmd.none)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick time -> (onTick model, Cmd.none)
    Key code -> onKey code model
    IncomingMessage message -> (handleIncomingMessage message model, Cmd.none)


board : Model -> String
board model =
  Board.emptyBoard width height
  |> Board.addSnake model.snake.body
  |> Board.addFood model.food
  |> AsciiBoard.boardToString


view : Model -> Html Msg
view model =
  Html.div [Att.class "container"]
  [ Html.pre [Att.class "board"] [ Html.text (board model) ]
  , Html.pre [Att.class "score"]
      [ Html.text <| toString <| List.length model.snake.body ]
  ]


subscriptions : Model -> Sub Msg
subscriptions model = Sub.batch
  [
  -- Time.every (Time.second/8) Tick
  Keyboard.downs Key
  , WebSocket.listen server IncomingMessage
  ]


boardSize = (30, 10)
width = Tuple.first boardSize
height = Tuple.second boardSize
boardIndex = (width-1, height-1)


init : (Model, Cmd Msg)
init = (Model ((Snake [(0,0)]) Right 4 True) (2,2) (Random.initialSeed 12345) 0, Cmd.none)


main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }
