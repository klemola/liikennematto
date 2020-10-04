module Lot exposing
    ( BuildingKind(..)
    , Lot(..)
    , allBuildingKinds
    , anchorCell
    , anchorDirection
    , anchorTo
    , cell
    , entryDirection
    , resident
    )

import Car exposing (CarKind(..))
import Cell exposing (Cell)
import Direction exposing (Direction(..))


type Lot
    = Building BuildingKind Anchor


type alias Anchor =
    -- road piece cell and direction from the road to the lot
    ( Cell, Direction )


type BuildingKind
    = ResidentialA
    | ResidentialB
    | ResidentialC
    | ResidentialD
    | ResidentialE


allBuildingKinds : List BuildingKind
allBuildingKinds =
    [ ResidentialA
    , ResidentialB
    , ResidentialC
    , ResidentialD
    , ResidentialE
    ]


entryDirection : BuildingKind -> Direction
entryDirection buildingKind =
    case buildingKind of
        ResidentialA ->
            Down

        ResidentialB ->
            Right

        ResidentialC ->
            Left

        ResidentialD ->
            Up

        ResidentialE ->
            Up


anchorDirection : BuildingKind -> Direction
anchorDirection buildingKind =
    entryDirection buildingKind
        |> Direction.opposite


anchorTo : BuildingKind -> ( Cell, a ) -> Lot
anchorTo buildingKind ( anchor, _ ) =
    let
        lot =
            Building buildingKind ( anchor, anchorDirection buildingKind )
    in
    lot


anchorCell : Lot -> Cell
anchorCell (Building _ ( anchor, _ )) =
    anchor


cell : Lot -> Cell
cell (Building _ ( roadPosition, dirFromRoad )) =
    Cell.next roadPosition dirFromRoad


resident : Lot -> CarKind
resident (Building buildingKind _) =
    case buildingKind of
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
