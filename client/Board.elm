module Board exposing (..)

import Matrix exposing (Matrix, map)


type Piece =
    Empty
  | SnakePart Int
  | Food


type alias Board = Matrix Piece


map : (a -> b) -> Matrix a -> Matrix b
map = Matrix.map

mapWithLocation : ((Int, Int) -> Piece -> b) -> Board -> List b
mapWithLocation f board =
  Matrix.mapWithLocation (\(y, x) piece -> f (x, y) piece) board
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


addSnake : Int -> List (Int, Int) -> Board -> Board
addSnake id = apply (SnakePart id)


addSnakes : List (Int, List (Int, Int)) -> Board -> Board
addSnakes snakes board =
  List.foldr (uncurry addSnake) board snakes


addFood : (Int, Int) -> Board -> Board
addFood = setValue Food


apply : Piece -> List (Int, Int) -> Board -> Board
apply piece positions board =
  List.foldr (setValue piece) board positions



