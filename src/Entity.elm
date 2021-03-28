module Entity exposing (Id, nextId)

import Dict exposing (Dict)


type alias Id =
    Int


nextId : Dict Id a -> Id
nextId dict =
    Dict.keys dict
        |> List.maximum
        |> Maybe.map ((+) 1)
        |> Maybe.withDefault 1
