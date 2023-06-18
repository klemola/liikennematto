module Data.Tiles exposing (allTiles, defaultTile)

import Model.Tile exposing (TileId, TileKind(..))
import Model.Tilemap exposing (Sockets, TerrainType(..), TileMeta)


allTiles : List TileMeta
allTiles =
    [ grass, horizontalRoad, verticalRoad, deadendUp, deadendRight, deadendDown, deadendLeft ]


defaultTile : TileMeta
defaultTile =
    grass


grass : TileMeta
grass =
    mkTile 0 Grass Grass Grass Grass


horizontalRoad : TileMeta
horizontalRoad =
    mkTile 6 Road Grass Road Grass


verticalRoad : TileMeta
verticalRoad =
    mkTile 9 Grass Road Grass Road



-- curveBottomRight : TileMeta
-- curveBottomRight =
--     3
-- curveBottomLeft : TileMeta
-- curveBottomLeft =
--     5
-- curveTopRight : TileMeta
-- curveTopRight =
--     10
-- curveTopLeft : TileMeta
-- curveTopLeft =
--     12


deadendDown : TileMeta
deadendDown =
    mkTile 1 Road Grass Grass Grass


deadendRight : TileMeta
deadendRight =
    mkTile 2 Grass Road Grass Grass


deadendLeft : TileMeta
deadendLeft =
    mkTile 4 Grass Grass Grass Road


deadendUp : TileMeta
deadendUp =
    mkTile 8 Grass Grass Road Grass



-- intersectionTUp : TileMeta
-- intersectionTUp =
--     7
-- intersectionTLeft : TileMeta
-- intersectionTLeft =
--     11
-- intersectionTRight : TileMeta
-- intersectionTRight =
--     13
-- intersectionTDown : TileMeta
-- intersectionTDown =
--     14
-- intersectionCross : TileMeta
-- intersectionCross =
--     15
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
    { top = ( t0, t3 )
    , left = ( t0, t1 )
    , bottom = ( t1, t2 )
    , right = ( t3, t2 )
    }
