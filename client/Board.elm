module Board exposing (..)

import Matrix exposing (Matrix, map)


type Piece =
    Empty
  | SnakePart
  | Food


type alias Board = Matrix Piece


map : (a -> b) -> Matrix a -> Matrix b
map = Matrix.map

mapWithLocation : ((Int, Int) -> Piece -> b) -> Board -> List b
mapWithLocation f board =
  Matrix.mapWithLocation f board
  |> Matrix.flatten

width = Matrix.colCount
height = Matrix.rowCount


toList : Matrix a -> List (List a)
toList = Matrix.toList


emptyBoard : Int -> Int -> Board
emptyBoard width height = Matrix.matrix height width (always Empty)


setValue : Piece -> ( Int, Int ) -> Board -> Board
setValue value pos matrix =
  case pos of
    (x,y) -> Matrix.set (y, x) value matrix


addSnake : List (Int, Int) -> Board -> Board
addSnake = apply SnakePart


addFood : (Int, Int) -> Board -> Board
addFood = setValue Food


apply : Piece -> List (Int, Int) -> Board -> Board
apply piece positions board =
  List.foldr (setValue piece) board positions



