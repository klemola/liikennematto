module Car exposing (Car, CarKind(..), Msg(..), Status(..), update, view)

import Collage exposing (Collage, image, rotate)
import Coords exposing (Coords)
import Direction exposing (Direction(..))
import Graphics exposing (texture)
import Tile exposing (Tile(..))


type CarKind
    = Sedan1
    | Sedan2
    | Sedan3
    | Sedan4
    | Sedan5


type Status
    = Moving
      -- Room for improvement: if the previous direction is included in "Turning", we can rotate the car half-step (45deg) when turning
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
    | YieldAtIntersection
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

        YieldAtIntersection ->
            { car | status = Yielding }

        StopAtIntersection turnsRemaining ->
            { car | status = StoppedAtIntersection turnsRemaining }


view : Float -> Car -> Collage msg
view size car =
    let
        asset =
            case car.kind of
                Sedan1 ->
                    "car_blue_1.png"

                Sedan2 ->
                    "car_red_1.png"

                Sedan3 ->
                    "car_green_1.png"

                Sedan4 ->
                    "car_yellow_1.png"

                Sedan5 ->
                    "car_black_1.png"
    in
    texture size asset
        |> rotate (degrees (Direction.rotationDegrees car.direction))
