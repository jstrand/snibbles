module AsciiBoard exposing (..)

import Board exposing (Board)


mapPiece : Board.Piece -> Char
mapPiece piece =
  case piece of
    Board.Empty -> '.'
    Board.SnakePart -> '*'


boardToString : Board -> String
boardToString =
  Board.map mapPiece
  >> Board.toList
  >> List.map String.fromList
  >> List.intersperse "\n"
  >> String.concat

