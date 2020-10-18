module Lot exposing
    ( Anchor
    , BuildingKind(..)
    , BuildingProperties
    , Lot(..)
    , allBuildings
    , anchorTo
    , entryCell
    , inBounds
    , resident
    )

import Car exposing (CarKind(..))
import Cell exposing (Cell)
import Config exposing (tileSize)
import Direction exposing (Direction(..))
import Position exposing (Position)


type Lot
    = Building BuildingProperties Position Anchor


type alias Anchor =
    -- road piece cell and direction from the road to the lot
    ( Cell, Direction )


type alias BuildingProperties =
    { kind : BuildingKind
    , width : Float
    , height : Float
    , entryDirection : Direction
    }


type BuildingKind
    = ResidentialA
    | ResidentialB
    | ResidentialC
    | ResidentialD
    | ResidentialE


allBuildings : List BuildingProperties
allBuildings =
    [ buildingProperties ResidentialA
    , buildingProperties ResidentialB
    , buildingProperties ResidentialC
    , buildingProperties ResidentialD
    , buildingProperties ResidentialE
    ]


anchorTo : BuildingProperties -> ( Cell, a ) -> Lot
anchorTo properties ( anchorCell, _ ) =
    let
        anchor =
            ( anchorCell, Direction.opposite properties.entryDirection )

        position =
            Tuple.second anchor
                |> Cell.next anchorCell
                |> Cell.bottomLeftCorner
    in
    Building properties position anchor


entryCell : Lot -> Cell
entryCell (Building _ _ ( roadPosition, dirFromRoad )) =
    Cell.next roadPosition dirFromRoad


inBounds : Cell -> Lot -> Bool
inBounds cell (Building props ( lotX, lotY ) _) =
    let
        ( cellX, cellY ) =
            Cell.bottomLeftCorner cell

        ( cellWidth, cellHeight ) =
            ( tileSize, tileSize )

        ( lotWidth, lotHeight ) =
            ( props.width, props.height )
    in
    (cellX < lotX + lotWidth)
        && (cellX + cellWidth > lotX)
        && (cellY < lotY + lotHeight)
        && (cellY + cellHeight > lotY)


buildingProperties : BuildingKind -> BuildingProperties
buildingProperties kind =
    case kind of
        ResidentialA ->
            BuildingProperties kind tileSize tileSize Down

        ResidentialB ->
            BuildingProperties kind tileSize tileSize Right

        ResidentialC ->
            BuildingProperties kind tileSize tileSize Left

        ResidentialD ->
            BuildingProperties kind tileSize tileSize Up

        ResidentialE ->
            BuildingProperties kind (2 * tileSize) (2 * tileSize) Down


resident : Lot -> CarKind
resident (Building { kind } _ _) =
    case kind of
        ResidentialA ->
            SedanA

        ResidentialB ->
            SedanB

        ResidentialC ->
            SedanC

        ResidentialD ->
            SedanD

        ResidentialE ->
            SedanE
