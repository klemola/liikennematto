module Model.Flags exposing
    ( Flags
    , FlagsJson
    , RuntimeEnvironment(..)
    , fromJsonValue
    )

import Json.Decode


type alias Flags =
    { runtimeEnvironment : RuntimeEnvironment
    }


type alias FlagsJson =
    Json.Decode.Value


type RuntimeEnvironment
    = Development
    | Release
    | Unknown


fallback : Flags
fallback =
    { runtimeEnvironment = Unknown }


fromJsonValue : Json.Decode.Value -> Flags
fromJsonValue value =
    value
        |> Json.Decode.decodeValue
            (Json.Decode.field "runtimeEnvironment"
                (Json.Decode.string
                    |> Json.Decode.andThen runtimeEnviromentFromString
                )
                |> Json.Decode.map Flags
            )
        |> Result.withDefault fallback


runtimeEnviromentFromString : String -> Json.Decode.Decoder RuntimeEnvironment
runtimeEnviromentFromString string =
    case string of
        "development" ->
            Json.Decode.succeed Development

        "release" ->
            Json.Decode.succeed Release

        _ ->
            Json.Decode.succeed Unknown
