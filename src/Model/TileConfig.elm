module Model.TileConfig exposing
    ( Socket(..)
    , Sockets
    , TileConfig
    , TileId
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
