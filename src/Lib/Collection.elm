module Lib.Collection exposing
    ( Collection
    , Id
    , addFromBuilder
    , addWithId
    , empty
    , find
    , foldl
    , get
    , idMatches
    , idToString
    , initialId
    , map
    , nextId
    , remove
    , size
    , update
    , values
    )

import Dict exposing (Dict)
import Dict.Extra as Dict


type Id
    = Id Int


type Collection a
    = Collection
        { collection : Dict Int a
        , idCounter : Int
        }


empty : Collection a
empty =
    Collection
        { collection = Dict.empty
        , idCounter = 0
        }


get : Id -> Collection a -> Maybe a
get (Id id) (Collection internals) =
    Dict.get id internals.collection


addFromBuilder : (Id -> a) -> Collection a -> ( a, Collection a )
addFromBuilder builderFn (Collection internals) =
    let
        nextIdValue =
            internals.idCounter + 1

        id =
            Id nextIdValue

        entity =
            builderFn id
    in
    ( entity
    , Collection
        { collection = Dict.insert nextIdValue entity internals.collection
        , idCounter = nextIdValue
        }
    )


remove : Id -> Collection a -> Collection a
remove (Id id) (Collection internals) =
    Collection
        { collection = Dict.remove id internals.collection
        , idCounter = internals.idCounter
        }


update : Id -> a -> Collection a -> Collection a
update (Id id) entity (Collection internals) =
    Collection
        { collection = Dict.insert id entity internals.collection
        , idCounter = internals.idCounter
        }


values : Collection a -> List a
values (Collection internals) =
    Dict.values internals.collection


map : (Id -> a -> b) -> Collection a -> Collection b
map fn (Collection internals) =
    Collection
        { collection = Dict.map (\idValue -> fn (Id idValue)) internals.collection
        , idCounter = internals.idCounter
        }


foldl : (Id -> a -> b -> b) -> b -> Collection a -> b
foldl fn acc (Collection internals) =
    Dict.foldl (\idValue -> fn (Id idValue)) acc internals.collection


find : (Id -> a -> Bool) -> Collection a -> Maybe ( Id, a )
find predicate (Collection internals) =
    Dict.find (\idValue -> predicate (Id idValue)) internals.collection
        |> Maybe.map (Tuple.mapFirst Id)


size : Collection a -> Int
size (Collection internals) =
    Dict.size internals.collection


idMatches : Id -> Id -> Bool
idMatches (Id a) (Id b) =
    a == b


idToString : Id -> String
idToString (Id id) =
    String.fromInt id



-- For tests and mock usage


addWithId : Id -> a -> Collection a -> Collection a
addWithId (Id id) entity (Collection internals) =
    Collection
        { collection = Dict.insert id entity internals.collection
        , idCounter = id
        }


initialId : Id
initialId =
    Id 1


nextId : Id -> Id
nextId (Id id) =
    Id (id + 1)
