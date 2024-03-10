module Tilemap.TileConfig exposing
    ( LargeTile
    , SingleTile
    , Socket(..)
    , Sockets
    , TileConfig(..)
    , TileId
    , allSockets
    , complexity
    , socketByDirection
    , socketByDirectionWithConfig
    , sockets
    , socketsList
    , tileConfigId
    , toString
    )

import Array exposing (Array)
import Lib.OrthogonalDirection exposing (OrthogonalDirection(..))
import List.Nonempty exposing (Nonempty)


type alias TileId =
    Int


type alias Sockets =
    { top : Socket
    , right : Socket
    , bottom : Socket
    , left : Socket
    }


type alias SingleTile =
    { id : TileId
    , sockets : Sockets
    , complexity : Float -- 0.0 to 1.0
    , baseTileId : Maybe TileId
    }


type alias LargeTile =
    { id : TileId
    , tiles : Array SingleTile
    , width : Int
    , height : Int
    , anchorIndex : Int
    , complexity : Float -- 0.0 to 1.0
    }


type TileConfig
    = Single SingleTile
    | Large LargeTile


type Socket
    = Red
    | Green
    | Blue
    | Pink
    | Yellow
    | Orange
    | Gray
    | White
    | LightBrown
    | DarkBrown


allSockets : List Socket
allSockets =
    [ Red
    , Green
    , Blue
    , Pink
    , Yellow
    , Orange
    , LightBrown
    , DarkBrown
    , Gray
    , White
    ]


tileConfigId : TileConfig -> TileId
tileConfigId tileConfig =
    case tileConfig of
        Single singleTile ->
            singleTile.id

        Large largeTile ->
            largeTile.id


complexity : TileConfig -> Float
complexity tileConfig =
    case tileConfig of
        Single singleTile ->
            singleTile.complexity

        Large largeTile ->
            largeTile.complexity


sockets : TileConfig -> Sockets
sockets tileConfig =
    case tileConfig of
        Single singleTile ->
            singleTile.sockets

        Large largeTile ->
            case Array.get largeTile.anchorIndex largeTile.tiles of
                Just anchorTile ->
                    anchorTile.sockets

                Nothing ->
                    { top = Green
                    , right = Green
                    , bottom = Green
                    , left = Green
                    }


socketsList : TileConfig -> Nonempty Socket
socketsList tileConfig =
    let
        sockets_ =
            sockets tileConfig
    in
    List.Nonempty.Nonempty sockets_.top [ sockets_.right, sockets_.bottom, sockets_.left ]


socketByDirectionWithConfig : TileConfig -> OrthogonalDirection -> Socket
socketByDirectionWithConfig tileConfig direction =
    let
        sockets_ =
            sockets tileConfig
    in
    socketByDirection sockets_ direction


socketByDirection : Sockets -> OrthogonalDirection -> Socket
socketByDirection sockets_ direction =
    case direction of
        Up ->
            sockets_.top

        Right ->
            sockets_.right

        Down ->
            sockets_.bottom

        Left ->
            sockets_.left


toString : TileConfig -> String
toString tileConfig =
    case tileConfig of
        Single singleTile ->
            "Single " ++ String.fromInt singleTile.id

        Large largeTile ->
            "Large " ++ String.fromInt largeTile.id
