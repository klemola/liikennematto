module Model.Animation exposing
    ( Animation
    , fromTilemapChange
    , isTileUpdate
    , update
    )

import Duration exposing (Duration)
import Model.Tilemap exposing (Cell, TileChange(..), TilemapChange)
import Quantity


type alias Animation =
    { duration : Duration
    , elapsed : Duration
    , kind : AnimationKind
    }


type AnimationKind
    = TileUpdate Cell TileAnimation


type TileAnimation
    = PopIn
    | Flash
    | Disappear


fromTilemapChange : TilemapChange -> List Animation
fromTilemapChange tilemapChange =
    tilemapChange.changedCells
        |> List.map
            (\( cell, tileChange ) ->
                case tileChange of
                    Add ->
                        Animation
                            (Duration.milliseconds 200)
                            Quantity.zero
                            (TileUpdate cell PopIn)

                    Remove ->
                        Animation
                            (Duration.milliseconds 100)
                            Quantity.zero
                            (TileUpdate cell Disappear)

                    Change ->
                        Animation
                            (Duration.milliseconds 100)
                            Quantity.zero
                            (TileUpdate cell Flash)
            )


update : Duration -> Animation -> Maybe Animation
update delta animation =
    let
        nextElapsed =
            animation.duration |> Quantity.plus delta
    in
    if nextElapsed |> Quantity.greaterThanOrEqualTo animation.duration then
        let
            _ =
                Debug.log "animation complete" animation.kind
        in
        Nothing

    else
        Just { animation | elapsed = nextElapsed }


isTileUpdate : Animation -> Bool
isTileUpdate animation =
    case animation.kind of
        TileUpdate _ _ ->
            True
