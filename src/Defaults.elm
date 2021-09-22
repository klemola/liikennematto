module Defaults exposing (..)

import Color
import Config exposing (tileSizeInMeters)
import Model.Car exposing (CarKind(..))
import Model.Cell exposing (OrthogonalDirection(..))
import Model.Lot exposing (Building, BuildingKind(..), Lot, NewLot)
import Quantity


lots : List NewLot
lots =
    [ { content = Building ResidentialA Down, width = tileSizeInMeters, height = tileSizeInMeters }
    , { content = Building ResidentialB Right, width = tileSizeInMeters, height = tileSizeInMeters }
    , { content = Building ResidentialC Left, width = tileSizeInMeters, height = tileSizeInMeters }
    , { content = Building ResidentialD Up, width = tileSizeInMeters, height = tileSizeInMeters }
    , { content = Building ResidentialE Down
      , width = tileSizeInMeters |> Quantity.multiplyBy 2
      , height = tileSizeInMeters |> Quantity.multiplyBy 2
      }
    , { content = Building TwoByOneTest Left
      , width = tileSizeInMeters |> Quantity.multiplyBy 2
      , height = tileSizeInMeters
      }
    , { content = Building ThreeByThreeTest Right
      , width = tileSizeInMeters |> Quantity.multiplyBy 3
      , height = tileSizeInMeters |> Quantity.multiplyBy 3
      }
    , { content = Building TwoByThreeTest Up
      , width = tileSizeInMeters |> Quantity.multiplyBy 2
      , height = tileSizeInMeters |> Quantity.multiplyBy 3
      }
    ]


resident : Lot -> Maybe CarKind
resident lot =
    case lot.content.kind of
        ResidentialA ->
            Just <|
                Sedan
                    { body = Color.rgb255 47 149 208
                    , detail = Color.rgb255 41 141 198
                    , shade = Color.rgb255 208 147 173
                    , edge = Color.rgb255 22 98 142
                    }

        ResidentialB ->
            Just <|
                Sedan
                    { body = Color.rgb255 232 106 23
                    , detail = Color.rgb255 191 100 40
                    , shade = Color.rgb255 217 163 125
                    , edge = Color.rgb255 159 73 16
                    }

        ResidentialC ->
            Just <|
                Sedan
                    { body = Color.rgb255 255 204 0
                    , detail = Color.rgb255 229 186 16
                    , shade = Color.rgb255 147 208 205
                    , edge = Color.rgb255 159 128 10
                    }

        ResidentialD ->
            Just <|
                Sedan
                    { body = Color.rgb255 57 194 114
                    , detail = Color.rgb255 48 182 104
                    , shade = Color.rgb255 147 208 205
                    , edge = Color.rgb255 20 119 61
                    }

        ResidentialE ->
            Just <|
                Sedan
                    { body = Color.rgb255 93 91 91
                    , detail = Color.rgb255 79 76 76
                    , shade = Color.rgb255 208 184 147
                    , edge = Color.rgb255 58 53 53
                    }

        _ ->
            Nothing
