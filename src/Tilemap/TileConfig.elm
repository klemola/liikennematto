module Tilemap.TileConfig exposing
    ( LargeTile
    , SingleTile
    , Socket(..)
    , Sockets
    , TileBiome(..)
    , TileConfig(..)
    , TileId
    , allSockets
    , biome
    , complexity
    , graphPriority
    , maxGraphPriority
    , mirroredHorizontally
    , mirroredVertically
    , rotatedClockwise
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


type TileConfig
    = Single SingleTile
    | Large LargeTile


type alias SingleTile =
    { id : TileId
    , complexity : Float -- 0.0 to 1.0
    , graphPriority : Float -- 0.0. to 1.0
    , biome : TileBiome
    , sockets : Sockets
    , baseTileId : Maybe TileId
    }


type alias LargeTile =
    { id : TileId
    , complexity : Float -- 0.0 to 1.0
    , biome : TileBiome
    , tiles : Array SingleTile
    , width : Int
    , height : Int
    , anchorIndex : Int
    }


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


type alias Sockets =
    { top : Socket
    , right : Socket
    , bottom : Socket
    , left : Socket
    }


type TileBiome
    = Road
    | Lot
    | Nature


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


maxGraphPriority : Float
maxGraphPriority =
    1.0


graphPriority : TileConfig -> Float
graphPriority tileConfig =
    case tileConfig of
        Single singleTile ->
            singleTile.graphPriority

        Large _ ->
            maxGraphPriority


biome : TileConfig -> TileBiome
biome tileConfig =
    case tileConfig of
        Single singleTile ->
            singleTile.biome

        Large largeTile ->
            largeTile.biome


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


mirroredHorizontally : TileId -> TileConfig -> TileConfig
mirroredHorizontally id tileConfig =
    case tileConfig of
        Single st ->
            let
                sockets_ =
                    st.sockets
            in
            Single
                { st
                    | id = id
                    , sockets = { sockets_ | left = sockets_.right, right = sockets_.left }
                }

        Large _ ->
            tileConfig


mirroredVertically : TileId -> TileConfig -> TileConfig
mirroredVertically id tileConfig =
    case tileConfig of
        Single st ->
            let
                sockets_ =
                    st.sockets
            in
            Single
                { st
                    | id = id
                    , sockets = { sockets_ | top = sockets_.bottom, bottom = sockets_.top }
                }

        Large _ ->
            tileConfig


rotatedClockwise : TileId -> TileConfig -> TileConfig
rotatedClockwise id tileConfig =
    case tileConfig of
        Single st ->
            Single
                { st
                    | id = id
                    , sockets =
                        { top = st.sockets.left
                        , right = st.sockets.top
                        , bottom = st.sockets.right
                        , left = st.sockets.bottom
                        }
                }

        Large _ ->
            tileConfig
