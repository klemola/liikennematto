module Model.Flags exposing
    ( Flags
    , FlagsJson
    , RuntimeEnvironment(..)
    , fallback
    , fromJsonValue
    )

import Json.Decode
import Json.Encode as JE
import Time


type alias Flags =
    { runtimeEnvironment : RuntimeEnvironment
    , time : Time.Posix
    , savegameData : Maybe JE.Value
    }


type alias FlagsJson =
    Json.Decode.Value


type RuntimeEnvironment
    = Development
    | Release
    | Unknown


fallback : Flags
fallback =
    { runtimeEnvironment = Unknown
    , time = Time.millisToPosix 42
    , savegameData = Nothing
    }


fromJsonValue : Json.Decode.Value -> Flags
fromJsonValue value =
    value
        |> Json.Decode.decodeValue flagsDecoder
        |> Result.withDefault fallback


flagsDecoder : Json.Decode.Decoder Flags
flagsDecoder =
    Json.Decode.map3 Flags
        (Json.Decode.field "runtimeEnvironment"
            (Json.Decode.string
                |> Json.Decode.andThen runtimeEnviromentFromString
            )
        )
        (Json.Decode.field "time" Json.Decode.int
            |> Json.Decode.map Time.millisToPosix
        )
        (Json.Decode.maybe (Json.Decode.field "savegameData" Json.Decode.value))


runtimeEnviromentFromString : String -> Json.Decode.Decoder RuntimeEnvironment
runtimeEnviromentFromString string =
    case string of
        "development" ->
            Json.Decode.succeed Development

        "release" ->
            Json.Decode.succeed Release

        _ ->
            Json.Decode.succeed Unknown
