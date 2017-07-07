module Palette exposing (snakeColor, foodColor)

import Color exposing (rgb, Color)
import Array exposing (Array)

-- Palette Dawnbringer 16
-- http://pixeljoint.com/forum/forum_posts.asp?TID=12795

dawnBringer16 =
  [ rgb 20 12 28
  , rgb 68 36 52
  , rgb 48 52 109
  , rgb 78 74 78
  , rgb 133 76 48
  , rgb 52 101 36
  , rgb 208 70 72
  , rgb 117 113 97
  , rgb 89 125 206
  , rgb 210 125 44
  , rgb 133 149 161
  , rgb 109 170 44
  , rgb 210 170 153
  , rgb 109 194 202
  , rgb 218 212 94
  , rgb 222 238 214
  ]
  |> Array.fromList


snakeColors =
  [ rgb 48 52 109
  , rgb 218 212 94
  , rgb 78 74 78
  , rgb 133 76 48
  , rgb 208 70 72
  , rgb 68 36 52
  , rgb 210 170 153
  , rgb 52 101 36
  , rgb 117 113 97
  , rgb 89 125 206
  , rgb 210 125 44
  , rgb 133 149 161
  , rgb 109 194 202
  , rgb 222 238 214
  ]
  |> Array.fromList


type alias ColorIndex = Int


getWithWrap : Int -> Array Color -> Color
getWithWrap index array =
  let
    wrap value = value % (Array.length array)
  in
    Array.get (wrap index) array
    |> Maybe.withDefault (rgb 20 12 28)


snakeColor : ColorIndex -> Color
snakeColor index = getWithWrap index snakeColors


foodColor : Color
foodColor = rgb 109 170 44

