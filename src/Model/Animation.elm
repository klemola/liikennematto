module Model.Animation exposing
    ( Animation
    , fromTilemapChange
    , isTileUpdate
    , keyframes
    , toCell
    , toStyleString
    , update
    )

import Duration exposing (Duration)
import Model.Tilemap exposing (Cell, TileChange(..), TilemapChange)
import Quantity


type alias Animation =
    { duration : Duration
    , elapsed : Duration
    , kind : AnimationKind
    , name : AnimationName
    }


type AnimationKind
    = TileUpdate Cell


type AnimationName
    = PopIn
    | Replace
    | Disappear


fromTilemapChange : TilemapChange -> List Animation
fromTilemapChange tilemapChange =
    tilemapChange.changedCells
        |> List.map
            (\( cell, tileChange ) ->
                case tileChange of
                    Add ->
                        Animation
                            (Duration.milliseconds 300)
                            Quantity.zero
                            (TileUpdate cell)
                            PopIn

                    Remove ->
                        Animation
                            (Duration.milliseconds 150)
                            Quantity.zero
                            (TileUpdate cell)
                            Disappear

                    Change ->
                        Animation
                            (Duration.milliseconds 700)
                            Quantity.zero
                            (TileUpdate cell)
                            Replace
            )


update : Duration -> Animation -> Maybe Animation
update delta animation =
    let
        nextElapsed =
            animation.elapsed |> Quantity.plus delta
    in
    if nextElapsed |> Quantity.greaterThanOrEqualTo animation.duration then
        Nothing

    else
        Just { animation | elapsed = nextElapsed }


isTileUpdate : Animation -> Bool
isTileUpdate animation =
    case animation.kind of
        TileUpdate _ ->
            True


toCell : Animation -> Maybe Cell
toCell animation =
    case animation.kind of
        TileUpdate tileCell ->
            Just tileCell


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
        12%  { transform: scale(1.05 , 1   )  rotate( 1.5deg); }
        25%  { transform: scale(1.1  , 1.05)  rotate( 2deg  ); }
        50%  { transform: scale(1    , 1   )  rotate( 0deg  ); }
        75%  { transform: scale(1.05 , 1.1 )  rotate(-2deg  ); }
        88%  { transform: scale(1    , 1.05)  rotate(-1.5deg); }
        100% { transform: scale(1    , 1   )  rotate( 0deg  ); }
    }

    @keyframes replace {
        65%   { transform: scale(1);  filter: blur(0px); opacity: 1;   }
        66%  { transform: scale(1.1); filter: blur(1px); opacity: 0.9; }
        100% { transform: scale(1);   filter: blur(0px); opacity: 1;   }
    }

    @keyframes disappear {
        0% {
            opacity: 1.0;
        }

        100% {
            opacity: 0;
        }
    }
    """
