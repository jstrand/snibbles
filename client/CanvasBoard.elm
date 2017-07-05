module CanvasBoard exposing (viewBoard)

import Collage exposing (..)
import Element exposing (Element)
import Color exposing (..)

import Board exposing (Board, Piece)


viewBoard : Board -> Element
viewBoard board = 
    let
        setup =
        { board = board
        , width = 500
        , height = 500
        }
    in
        collage setup.width setup.height (viewSnakes setup)
        --collage setup.width setup.height [snake setup Color.blue (0,0), snake setup Color.red (29,29), snake setup Color.green (29, 29) ]
type alias Setup =
    { board: Board
    , width: Int
    , height: Int
    }


viewSnakes : Setup -> List Form
viewSnakes setup =
    Board.mapWithLocation (pieceToElement setup) setup.board


pieceToElement : Setup -> (Int, Int) -> Piece -> Form
pieceToElement setup (y, x) piece =
    case piece of
        Board.SnakePart -> snake setup Color.red (x, y)
        _ -> toForm Element.empty


tileWidth setup = (toFloat setup.width) / (toFloat (Board.width setup.board))
tileHeight setup = (toFloat setup.height) / (toFloat (Board.height setup.board))
tileSize setup = (tileWidth setup, tileHeight setup)

positionToCoordinate : Setup -> (Int, Int) -> (Float, Float)
positionToCoordinate setup (x, y) =
    let
        offsetX = -((toFloat setup.width) / 2 - (tileWidth setup) / 2)
        offsetY = (toFloat setup.height) / 2 - (tileHeight setup) / 2
        xf = toFloat x
        yf = toFloat y
    in
        (xf * (tileWidth setup) + offsetX, yf * -(tileHeight setup) + offsetY)


snake : Setup -> Color -> (Int, Int) -> Form
snake setup color position =
    let
        coordinates = positionToCoordinate setup position
    in
        rect (tileWidth setup) (tileHeight setup)
        |> filled color
        |> move coordinates