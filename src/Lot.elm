module Lot exposing
    ( Anchor
    , BuildingKind(..)
    , BuildingProperties
    , Lot(..)
    , adjustOriginByAnchor
    , allBuildings
    , anchorTo
    , boundingBox
    , entryCell
    , inBounds
    , resident
    )

import Car exposing (CarKind(..))
import Cell exposing (Cell)
import Collision exposing (BoundingBox)
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
    | TwoByOneTest
    | ThreeByThreeTest
    | TwoByThreeTest


allBuildings : List BuildingProperties
allBuildings =
    [ buildingProperties ResidentialA
    , buildingProperties ResidentialB
    , buildingProperties ResidentialE
    , buildingProperties ResidentialC
    , buildingProperties TwoByOneTest
    , buildingProperties ResidentialD
    , buildingProperties TwoByThreeTest
    , buildingProperties ThreeByThreeTest
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
                |> adjustOriginByAnchor properties
    in
    Building properties position anchor


adjustOriginByAnchor : BuildingProperties -> Position -> Position
adjustOriginByAnchor { entryDirection, width, height } origin =
    let
        anchorDirection =
            Direction.opposite entryDirection
    in
    case anchorDirection of
        Up ->
            origin

        Right ->
            origin

        Down ->
            ( Tuple.first origin - (width - tileSize)
            , Tuple.second origin - (height - tileSize)
            )

        Left ->
            ( Tuple.first origin - (width - tileSize)
            , Tuple.second origin - (height - tileSize)
            )


entryCell : Lot -> Cell
entryCell (Building _ _ ( roadPosition, dirFromRoad )) =
    Cell.next roadPosition dirFromRoad


boundingBox : Lot -> BoundingBox
boundingBox (Building props ( lotX, lotY ) _) =
    { x = lotX, y = lotY, width = props.width, height = props.height }


inBounds : Cell -> Lot -> Bool
inBounds cell lot =
    Collision.aabb (Cell.boundingBox cell) (boundingBox lot)


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

        TwoByOneTest ->
            BuildingProperties kind (2 * tileSize) tileSize Left

        ThreeByThreeTest ->
            BuildingProperties kind (3 * tileSize) (3 * tileSize) Right

        TwoByThreeTest ->
            BuildingProperties kind (2 * tileSize) (3 * tileSize) Up


resident : Lot -> Maybe CarKind
resident (Building { kind } _ _) =
    case kind of
        ResidentialA ->
            Just SedanA

        ResidentialB ->
            Just SedanB

        ResidentialC ->
            Just SedanC

        ResidentialD ->
            Just SedanD

        ResidentialE ->
            Just SedanE

        TwoByOneTest ->
            Nothing

        ThreeByThreeTest ->
            Nothing

        TwoByThreeTest ->
            Nothing
