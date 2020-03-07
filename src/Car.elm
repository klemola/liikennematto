module Car exposing (Car, CarKind(..), Msg(..), Status(..), update, view)

import Collage exposing (Collage, image, rotate)
import Coords exposing (Coords)
import Direction exposing (Direction(..))
import Tile exposing (Tile(..))


type CarKind
    = Sedan1
    | Sedan2
    | Sedan3
    | Sedan4
    | Sedan5


type Status
    = Moving
    | Turning
    | Waiting
    | StoppedAtIntersection Int
    | Yielding


type alias Car =
    { coords : Coords
    , direction : Direction
    , kind : CarKind
    , status : Status
    }


type alias Model =
    Car


type Msg
    = Move
    | Turn Direction
    | Wait
    | Yield
    | StopAtIntersection Int


update : Msg -> Model -> Model
update msg car =
    case msg of
        Move ->
            let
                nextCoords =
                    Coords.next car.coords car.direction
            in
            { car | coords = nextCoords, status = Moving }

        Turn dir ->
            { car | direction = dir, status = Turning }

        Wait ->
            { car | status = Waiting }

        Yield ->
            { car | status = Yielding }

        StopAtIntersection turnsRemaining ->
            { car | status = StoppedAtIntersection turnsRemaining }


view : Float -> Car -> Collage msg
view tileSize car =
    let
        size =
            tileSize / 2

        asset =
            case car.kind of
                Sedan1 ->
                    "assets/car_blue_1.png"

                Sedan2 ->
                    "assets/car_red_1.png"

                Sedan3 ->
                    "assets/car_green_1.png"

                Sedan4 ->
                    "assets/car_yellow_1.png"

                Sedan5 ->
                    "assets/car_black_1.png"

        carShape =
            image ( size, size ) asset
    in
    carShape
        |> rotate (degrees (Direction.rotationDegrees car.direction))
