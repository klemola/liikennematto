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
import Model.Tilemap as Tilemap
    exposing
        ( Cell
        , OrthogonalDirection(..)
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
    , direction : Maybe OrthogonalDirection
    }


type AnimationKind
    = TileUpdate Cell Tile


type AnimationName
    = Appear
    | Replace
    | Disappear


tileAnimationDuration : Duration
tileAnimationDuration =
    Duration.milliseconds 250


fromTilemapChange : TilemapChange -> List Animation
fromTilemapChange tilemapChange =
    tilemapChange.changedCells
        |> List.filterMap
            (\( { cell, currentTile, nextTile }, tileChange ) ->
                case tileChange of
                    Add ->
                        Just
                            { duration = tileAnimationDuration
                            , elapsed = Quantity.zero
                            , kind = TileUpdate cell nextTile
                            , name = Appear
                            , direction = animationDirectionFromTile nextTile
                            }

                    Remove ->
                        Just
                            { duration = tileAnimationDuration
                            , elapsed = Quantity.zero
                            , kind = TileUpdate cell currentTile
                            , name = Disappear
                            , direction = Nothing
                            }

                    Change ->
                        Nothing
            )


animationDirectionFromTile : Tile -> Maybe OrthogonalDirection
animationDirectionFromTile tile =
    case Tilemap.potentialConnections tile of
        [ connection ] ->
            Just (Tilemap.oppositeOrthogonalDirection connection)

        _ ->
            Nothing


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
    let
        name =
            keyframeName animation

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
        , ";"
        ]


keyframeName : Animation -> String
keyframeName animation =
    case ( animation.name, animation.direction ) of
        ( Appear, Just _ ) ->
            "appear-from-direction"

        ( Appear, Nothing ) ->
            "appear-directionless"

        ( Replace, _ ) ->
            "replace"

        ( Disappear, _ ) ->
            "disappear"


keyframes : String
keyframes =
    """
    @keyframes appear-from-direction {
        0%   { transform: translate(var(--xOffset), var(--yOffset)); }
        50%  { transform: translate(0,            , 0             ); }
        51%  { transform: scale(    1             , 1             ); }
        80%  { transform: scale(    var(--scaleX) , var(--scaleY) ); }
        100% { transform: scale(    1             , 1             ); }
    }

    @keyframes appear-directionless {
        0%   { transform: scale(1    , 1   ) rotate( 0deg  ); }
        15%  { transform: scale(1.05 , 1   ) rotate( 1.5deg); }
        25%  { transform: scale(1.1  , 1.05) rotate( 2deg  ); }
        50%  { transform: scale(1    , 1   ) rotate( 0deg  ); }
        75%  { transform: scale(1.05 , 1.1 ) rotate(-2deg  ); }
        85%  { transform: scale(1    , 1.05) rotate(-1.5deg); }
        97%  { transform: scale(1    , 1   ) rotate( 0deg  ); }
        100% { transform: scale(1    , 1   ) rotate( 0deg  ); }
    }

    @keyframes disappear {
        0%   { transform: scale(1  ); opacity: 1.0; }
        98%  { transform: scale(1.5); opacity: 0.0; }
        100% { transform: scale(1.5); opacity: 0.0; }
    }
    """
