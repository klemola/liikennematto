module Lot exposing
    ( BuildingKind(..)
    , Lot(..)
    , allBuildingKinds
    , anchorDirection
    , anchorPosition
    , anchorTo
    , entryDirection
    , position
    )

import Direction exposing (Direction(..))
import Position exposing (Position)


type Lot
    = Building BuildingKind Anchor


type alias Anchor =
    -- road piece position and direction from the road to the lot
    ( Position, Direction )


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


anchorTo : BuildingKind -> ( Position, a ) -> ( Lot, Position )
anchorTo buildingKind ( anchor, _ ) =
    let
        lot =
            Building buildingKind ( anchor, anchorDirection buildingKind )
    in
    ( lot, anchor )


anchorPosition : Lot -> Position
anchorPosition (Building _ ( anchor, _ )) =
    anchor


position : Lot -> Position
position (Building _ ( roadPosition, dirFromRoad )) =
    Position.next roadPosition dirFromRoad
