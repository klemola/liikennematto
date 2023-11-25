module Model.TileConfig exposing
    ( LargeTile
    , SingleTile
    , Socket(..)
    , Sockets
    , TileConfig(..)
    , TileId
    , allSockets
    , isSingleTile
    , socketByDirection
    , socketByDirectionWithConfig
    , sockets
    , tileConfigId
    , toString
    )

import Array exposing (Array)
import Model.Geometry exposing (OrthogonalDirection(..))


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
    }


type alias LargeTile =
    { id : TileId
    , tiles : Array SingleTile
    , width : Int
    , height : Int
    , anchorIndex : Int
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


tileConfigId : TileConfig -> Int
tileConfigId tileConfig =
    case tileConfig of
        Single singleTile ->
            singleTile.id

        Large largeTile ->
            largeTile.id


isSingleTile : TileConfig -> Bool
isSingleTile tileConfig =
    case tileConfig of
        Single _ ->
            True

        Large _ ->
            False


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
                    -- TODO: placeholder value
                    { top = Green
                    , right = Green
                    , bottom = Green
                    , left = Green
                    }


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
