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


applyIncomingMessage : Game -> String -> Game
applyIncomingMessage game message =
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


performGameStep : Game -> String -> Game
performGameStep game message =
  applyIncomingMessage game message
  |> moveSnakes


update : Msg -> Game -> (Game, Cmd Msg)
update msg game =
  case msg of
    Tick time -> (onTick game, Cmd.none)
    Key code -> onKey code game
    IncomingMessage message -> (performGameStep game message, Cmd.none)


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


main =
  Html.program
    { init =
        addSnake 1 (0,0) emptyGame
        ! []
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


moveOneSnake : Game -> Int -> Snake -> Snake
moveOneSnake game id snake =
  let
    movedSnake = Snake.move snake boardIndex
  in
    movedSnake
    |> applySnakeCollision game


moveSnakes : Game -> Game
moveSnakes game =
  let
    movedSnakes = Dict.map (moveOneSnake game) game.snakes
  in
    { game | snakes = movedSnakes }


applySnakeCollision : Game -> Snake -> Snake
applySnakeCollision game snake =
  let
    snakeHead = Snake.head snake
    eating = snakeHead == game.food
    collided = detectCollision snakeHead (Snake.tail snake)
    applyEffect =
      if eating then
        Snake.grow
      else if collided then
        Snake.kill
      else
       (\x -> x)
  in
    applyEffect snake

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
