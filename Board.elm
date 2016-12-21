module Board exposing (..)

import Matrix exposing (Matrix)


type Piece =
    Empty
  | SnakePart


type alias Board = Matrix Piece


map = Matrix.map
toList = Matrix.toList


emptyBoard width height = Matrix.matrix height width (always Empty)


setValue value pos matrix =
  case pos of
    (x,y) -> Matrix.set (y, x) value matrix


addSnake : List (Int, Int) -> Board -> Board
addSnake = apply SnakePart


apply : Piece -> List (Int, Int) -> Board -> Board
apply piece positions board =
  List.foldr (setValue piece) board positions



