module Model.Animation exposing
    ( Animation
    , fromTilemapChange
    , isTileUpdate
    , keyframes
    , tileAnimationDuration
    , toCell
    , toStyleString
    , toTile
    , update
    )

import Duration exposing (Duration)
import Model.Tilemap
    exposing
        ( Cell
        , Tile
        , TileOperation(..)
        , TilemapChange
        )
import Quantity


type alias Animation =
    { duration : Duration
    , elapsed : Duration
    , kind : AnimationKind
    , name : AnimationName
    }


type AnimationKind
    = TileUpdate Cell Tile


type AnimationName
    = PopIn
    | Replace
    | Disappear


tileAnimationDuration : Duration
tileAnimationDuration =
    Duration.milliseconds 300


tileChangeDuration : Duration
tileChangeDuration =
    tileAnimationDuration
        |> Quantity.twice
        -- extra wait time
        |> Quantity.plus (Duration.milliseconds 100)


fromTilemapChange : TilemapChange -> List Animation
fromTilemapChange tilemapChange =
    tilemapChange.changedCells
        |> List.map
            (\( { cell, currentTile, nextTile }, tileChange ) ->
                case tileChange of
                    Add ->
                        Animation
                            tileAnimationDuration
                            Quantity.zero
                            (TileUpdate cell nextTile)
                            PopIn

                    Remove ->
                        Animation
                            tileAnimationDuration
                            Quantity.zero
                            (TileUpdate cell currentTile)
                            Disappear

                    Change ->
                        Animation
                            tileChangeDuration
                            Quantity.zero
                            (TileUpdate cell nextTile)
                            Replace
            )


update : Duration -> Animation -> Maybe Animation
update delta animation =
    let
        nextElapsed =
            animation.elapsed |> Quantity.plus delta

        paddedDuration =
            -- Make sure that the animation can be completed
            animation.duration |> Quantity.plus delta
    in
    if nextElapsed |> Quantity.greaterThan paddedDuration then
        Nothing

    else
        Just { animation | elapsed = nextElapsed }


isTileUpdate : Animation -> Bool
isTileUpdate animation =
    case animation.kind of
        TileUpdate _ _ ->
            True


toCell : Animation -> Maybe Cell
toCell animation =
    case animation.kind of
        TileUpdate tileCell _ ->
            Just tileCell


toTile : Animation -> Maybe Tile
toTile animation =
    case animation.kind of
        TileUpdate _ tile ->
            Just tile


toStyleString : Animation -> String
toStyleString animation =
    -- TODO return Maybe String once delay is implemented, if there's delay left
    let
        name =
            nameToString animation.name

        duration =
            (animation.duration
                |> Duration.inMilliseconds
                |> String.fromFloat
            )
                ++ "ms"
    in
    String.join " "
        [ "animation:"
        , duration
        , name
        , "ease-in-out"
        , ";"
        ]


nameToString : AnimationName -> String
nameToString name =
    case name of
        PopIn ->
            "pop-in"

        Replace ->
            "replace"

        Disappear ->
            "disappear"


keyframes : String
keyframes =
    """
    @keyframes pop-in {
        0%   { transform: scale(1    , 1   )  rotate( 0deg  ); }
        15%  { transform: scale(1.05 , 1   )  rotate( 1.5deg); }
        25%  { transform: scale(1.1  , 1.05)  rotate( 2deg  ); }
        50%  { transform: scale(1    , 1   )  rotate( 0deg  ); }
        75%  { transform: scale(1.05 , 1.1 )  rotate(-2deg  ); }
        85%  { transform: scale(1    , 1.05)  rotate(-1.5deg); }
        97%  { transform: scale(1    , 1   )  rotate( 0deg  ); }
        100% { transform: scale(1    , 1   )  rotate( 0deg  ); }
    }

    @keyframes replace {
        65%  { transform: scale(1);   }
        66%  { transform: scale(1.05); }
        100% { transform: scale(1);   }
    }

    @keyframes disappear {
        0%   { transform: scale(1);   opacity: 1.0; }
        66%  { transform: scale(1.5); opacity: 0.0; }
        100% { transform: scale(1.5); opacity: 0.0; }
    }
    """
