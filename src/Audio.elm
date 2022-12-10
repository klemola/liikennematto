port module Audio exposing
    ( Sound(..)
    , onAudioInitComplete
    , playSound
    )


type Sound
    = BuildRoadStart
    | BuildRoadEnd
    | DestroyRoad
    | BuildLot


playSound : Sound -> Cmd msg
playSound sound =
    let
        label =
            case sound of
                BuildRoadStart ->
                    "build_road_start"

                BuildRoadEnd ->
                    "build_road_end"

                DestroyRoad ->
                    "destroy_road"

                BuildLot ->
                    "build_lot"
    in
    requestAudio label


port requestAudio : String -> Cmd msg


port onAudioInitComplete : (String -> msg) -> Sub msg
