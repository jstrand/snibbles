module Snibbles exposing (..)

import Snake exposing (..)
import Game exposing (..)
import Board
import CanvasBoard exposing (viewBoard)
import Element exposing (toHtml)
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
    gameWithMovedSnakes = Game.moveSnakes game
  in
    { gameWithMovedSnakes | ticks = game.ticks + 1 }


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


createSnake : ServerMessage -> Game -> Game
createSnake (id, dir) game = Game.addSnake id dir game


setupGame : Game -> List ServerMessage -> Game
setupGame game messages =
  let
    existingSnakeIds = List.sort <| Dict.keys game.snakes
    newSnakeIds = List.sort <| List.map Tuple.first messages
    snakeSetHasChanged = existingSnakeIds /= newSnakeIds
  in
    if snakeSetHasChanged then
      List.foldl createSnake emptyGame messages
    else
      game


applyIncomingMessage : Game -> String -> Game
applyIncomingMessage game message =
  let
    parsedMessage = decodeString messageDecoder message
  in
    case parsedMessage of
      Ok newDirections -> List.foldr (uncurry Game.changeDir) (setupGame game newDirections) newDirections
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


gameToBoard : Game -> Board.Board
gameToBoard model =
  Board.emptyBoard width height
  |> Board.addSnake (snakePartPositions model)
  |> Board.addFood model.food


gameToHtml : Game -> Html Msg
gameToHtml =
  gameToBoard
  >> viewBoard
  >> toHtml


view : Game -> Html Msg
view game =
  Html.div [Att.class "container", Att.style [("margin", "10px")]]
  [ gameToHtml game ]


subscriptions : Game -> Sub Msg
subscriptions model = Sub.batch
  [
  -- Time.every (Time.second/8) Tick
  Keyboard.downs Key
  , WebSocket.listen server IncomingMessage
  ]


main =
  Html.program
    { init = emptyGame -- addSnake 1 (0,0) emptyGame
        ! []
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


