module Data.Tiles exposing
    ( allTiles
    , defaultTile
    )

import Model.Tile exposing (TileId)
import Model.Tilemap exposing (Sockets, TerrainType(..), TileMeta)


allTiles : List TileMeta
allTiles =
    [ grass

    -- , loneRoad
    , horizontalRoad
    , verticalRoad
    , deadendUp
    , deadendDown
    , deadendRight
    , deadendLeft
    , curveBottomRight
    , curveBottomLeft
    , curveTopRight
    , curveTopLeft
    , intersectionTUp
    , intersectionTLeft
    , intersectionTRight
    , intersectionTDown
    , intersectionCross
    ]


defaultTile : TileMeta
defaultTile =
    grass


grass : TileMeta
grass =
    mkTile 0 Grass Grass Grass Grass


loneRoad : TileMeta
loneRoad =
    mkTile 17 Grass Grass Grass Grass


horizontalRoad : TileMeta
horizontalRoad =
    mkTile 6 Grass Road Grass Road


verticalRoad : TileMeta
verticalRoad =
    mkTile 9 Road Grass Road Grass


curveBottomRight : TileMeta
curveBottomRight =
    mkTile 3 Road Grass Grass Road


curveBottomLeft : TileMeta
curveBottomLeft =
    mkTile 5 Road Road Grass Grass


curveTopRight : TileMeta
curveTopRight =
    mkTile 10 Grass Grass Road Road


curveTopLeft : TileMeta
curveTopLeft =
    mkTile 12 Grass Road Road Grass


deadendDown : TileMeta
deadendDown =
    mkTile 1 Road Grass Grass Grass


deadendRight : TileMeta
deadendRight =
    mkTile 2 Grass Grass Grass Road


deadendLeft : TileMeta
deadendLeft =
    mkTile 4 Grass Road Grass Grass


deadendUp : TileMeta
deadendUp =
    mkTile 8 Grass Grass Road Grass


intersectionTUp : TileMeta
intersectionTUp =
    mkTile 7 Road Road Grass Road


intersectionTLeft : TileMeta
intersectionTLeft =
    mkTile 11 Road Grass Road Road


intersectionTRight : TileMeta
intersectionTRight =
    mkTile 13 Road Road Road Grass


intersectionTDown : TileMeta
intersectionTDown =
    mkTile 14 Grass Road Road Road


intersectionCross : TileMeta
intersectionCross =
    mkTile 15 Road Road Road Road



-- lotEntryTUp : TileMeta
-- lotEntryTUp =
-- 23
-- lotEntryTLeft : TileMeta
-- lotEntryTLeft =
--     27
-- lotEntryTRight : TileMeta
-- lotEntryTRight =
--     29
--


mkTile : TileId -> TerrainType -> TerrainType -> TerrainType -> TerrainType -> TileMeta
mkTile id t0 t1 t2 t3 =
    { id = id
    , sockets = mkSockets t0 t1 t2 t3
    }


mkSockets : TerrainType -> TerrainType -> TerrainType -> TerrainType -> Sockets
mkSockets t0 t1 t2 t3 =
    { top = t0
    , right = t1
    , bottom = t2
    , left = t3
    }
