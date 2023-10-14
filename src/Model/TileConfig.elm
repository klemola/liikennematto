module Model.TileConfig exposing
    ( Socket(..)
    , Sockets
    , TileConfig
    , TileId
    , allSockets
    )


type alias TileId =
    Int


type alias Sockets =
    { top : Socket
    , right : Socket
    , bottom : Socket
    , left : Socket
    }


type alias TileConfig =
    { id : TileId
    , sockets : Sockets
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


allSockets : List Socket
allSockets =
    [ Red
    , Green
    , Blue
    , Pink
    , Yellow
    , Orange
    , Gray
    , White
    , LightBrown
    , DarkBrown
    ]
