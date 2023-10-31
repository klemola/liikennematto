module Model.TileConfig exposing
    ( LargeTile
    , SingleTile
    , Socket(..)
    , Sockets
    , TileConfig(..)
    , TileId
    , allSockets
    , socketByDirection
    , sockets
    , tileConfigId
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
    , tiles :
        Array
            { id : TileId
            , sockets : Sockets
            }
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
    , Gray
    , White
    , LightBrown
    , DarkBrown
    ]


tileConfigId : TileConfig -> Int
tileConfigId tileConfig =
    case tileConfig of
        Single singleTile ->
            singleTile.id

        Large largeTile ->
            largeTile.id


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


socketByDirection : TileConfig -> OrthogonalDirection -> Socket
socketByDirection tileConfig direction =
    let
        sockets_ =
            sockets tileConfig
    in
    case direction of
        Up ->
            sockets_.top

        Right ->
            sockets_.right

        Down ->
            sockets_.bottom

        Left ->
            sockets_.left
