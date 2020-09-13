module Lot exposing
    ( BuildingKind(..)
    , Lot(..)
    , allBuildingKinds
    , anchorDirection
    , anchorTo
    , coords
    , entryDirection
    )

import Coords exposing (Coords)
import Direction exposing (Direction(..))


type Lot
    = Building BuildingKind Anchor


type alias Anchor =
    -- road piece coords and direction from the road to the lot
    ( Coords, Direction )


type BuildingKind
    = ResidentialA
    | ResidentialB


allBuildingKinds : List BuildingKind
allBuildingKinds =
    [ ResidentialA, ResidentialB ]


entryDirection : BuildingKind -> Direction
entryDirection buildingKind =
    case buildingKind of
        ResidentialA ->
            Down

        ResidentialB ->
            Right


anchorDirection : BuildingKind -> Direction
anchorDirection buildingKind =
    entryDirection buildingKind
        |> Direction.opposite


anchorTo : BuildingKind -> ( Coords, a ) -> ( Lot, Coords )
anchorTo buildingKind ( anchor, _ ) =
    let
        lot =
            Building buildingKind ( anchor, anchorDirection buildingKind )
    in
    ( lot, anchor )


coords : Lot -> Coords
coords (Building _ ( roadCoords, dirFromRoad )) =
    Coords.next roadCoords dirFromRoad
