module Snibbles exposing (..)

import Snake exposing (..)
import Game exposing (..)
import Board
import AsciiBoard
import Dict

import Html exposing (Html)
import Html.Attributes as Att
import Time exposing (Time)
import Keyboard
import WebSocket
import Json.Decode exposing (..)


type Msg =
    Tick Time
  | Key Keyboard.KeyCode
  | IncomingMessage String


server = "ws://localhost:9000"


onTick : Game -> Game
onTick game =
  let
    gameWithMovedSnakes = moveSnakes game
  in
    { gameWithMovedSnakes | ticks = game.ticks + 1 }


changeDir : Int -> Direction -> Game -> Game
changeDir snakeId dir model =
  let
    snake = Dict.get snakeId model.snakes
    movedSnake = Maybe.map (\snake -> Snake.changeDir snake dir) snake
    newSnakes = Maybe.map (\movedSnake -> Dict.insert snakeId movedSnake model.snakes) movedSnake
    newModel = Maybe.map (\newSnakes -> { model | snakes = newSnakes}) newSnakes
  in
    Maybe.withDefault model newModel


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


type alias ServerMessage = (Int, Snake.Direction)


snakeIdDecoder = Json.Decode.field "id" int
snakeMsgDecoder = Json.Decode.field "msg" string
snakeDirDecoder = Json.Decode.map stringAsDir snakeMsgDecoder
messageDecoder = Json.Decode.list (Json.Decode.map2 (,) snakeIdDecoder snakeDirDecoder)


handleIncomingMessage : String -> Game -> Game
handleIncomingMessage message game =
  let
    parsedMessage = decodeString messageDecoder message
  in
    case parsedMessage of
      Ok newDirections -> List.foldr (uncurry changeDir) game newDirections
      Err reason -> { game | error = reason }


sendDir dir = WebSocket.send server (dirAsString dir)


onKey code model =
  case code of
    37 -> (model, sendDir Snake.Left)
    39 -> (model, sendDir Snake.Right)
    38 -> (model, sendDir Snake.Up)
    40 -> (model, sendDir Snake.Down)
    _ -> (model, Cmd.none)


update : Msg -> Game -> (Game, Cmd Msg)
update msg model =
  case msg of
    Tick time -> (onTick model, Cmd.none)
    Key code -> onKey code model
    IncomingMessage message -> (moveSnakes (handleIncomingMessage message model), Cmd.none)


board : Game -> String
board model =
  Board.emptyBoard width height
  |> Board.addSnake (snakePartPositions model)
  |> Board.addFood model.food
  |> AsciiBoard.boardToString


view : Game -> Html Msg
view model =
  Html.div [Att.class "container"]
  [ Html.pre [Att.class "board"] [ Html.text (board model) ]
  --, Html.pre [Att.class "score"]
  --    [ Html.text <| toString <| List.length model.snake.body ]
  ]


subscriptions : Game -> Sub Msg
subscriptions model = Sub.batch
  [
  -- Time.every (Time.second/8) Tick
  Keyboard.downs Key
  , WebSocket.listen server IncomingMessage
  ]


--init : (Game, Cmd Msg)
--init = (Game ((Snake [(0,0)]) Right 4 True) (2,2) (Random.initialSeed 12345) 0, Cmd.none)


main =
  Html.program
    { init =
        addSnake 1 (0,0) emptyGame
        ! []
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


moveSnakes : Game -> Game
moveSnakes model = moveSnakeById 1 model


moveSnakeById : Int -> Game -> Game
moveSnakeById snakeId game =
  Dict.get snakeId game.snakes
  |> Maybe.map (\snake -> moveSnake snake snakeId game)
  |> Maybe.withDefault game


moveSnake : Snake -> Int -> Game -> Game
moveSnake snake snakeId game =
  moveSnakeOnly snake snakeId game


moveSnakeOnly : Snake -> Int -> Game -> Game
moveSnakeOnly snake snakeId game =
  let
    movedSnake = Snake.move snake boardIndex
  in
    updateSnake snakeId movedSnake game


moveSnakeWithCollision snake model = model

{--
moveSnake snakeId model =
  let
      snake = Dict.get snakeId model.snakes
      movedSnake = Snake.move snake boardIndex
      movedHead = Snake.head movedSnake
      eating = movedHead == model.food
      collided = detectCollision movedHead (Snake.tail movedSnake)
      growingSnake = Snake.grow movedSnake
      modelMoved = updateSnake snakeId movedSnake model
      modelGrowing = updateSnake snakeId growingSnake model
      (nextFood, seed) = nextFoodPosition model.seed (obstacles modelMoved)
  in
    if collided then
      updateSnake snakeId (Snake.kill snake) model
    else if eating then
      { modelGrowing
      | food = nextFood
      , seed = seed
      }
    else
      modelMoved
--}
