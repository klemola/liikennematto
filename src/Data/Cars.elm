module Data.Cars exposing
    ( CarMake
    , CarRole(..)
    , CarStyle
    , carStyleToString
    , fireTruck
    , hatchback
    , randomCarMake
    , sedan
    , testCar
    , van
    )

import Color exposing (Color)
import Common exposing (GlobalCoordinates)
import Data.Colors as Colors
import Length exposing (Length)
import Point2d
import Polygon2d exposing (Polygon2d)
import Quantity
import Random
import Random.Extra


type alias CarMake =
    { bodyColor : Color
    , accentColor : Color
    , length : Length
    , width : Length
    , shapeAtOrigin : Polygon2d Length.Meters GlobalCoordinates
    , style : CarStyle
    , role : CarRole
    }


type CarStyle
    = Sedan
    | Hatchback
    | Van
    | Firetruck


type CarRole
    = None
    | ServiceVehicle


commonCarStyles : List CarStyle
commonCarStyles =
    [ Sedan, Hatchback, Van ]


testCar : CarMake
testCar =
    sedan Colors.gray5 Colors.gray3


randomCarMake : Random.Generator CarMake
randomCarMake =
    let
        bodyColor =
            Colors.gray5

        accentColor =
            Colors.gray3
    in
    Random.Extra.sample commonCarStyles
        |> Random.map (Maybe.withDefault Sedan)
        |> Random.map
            (\carStyle ->
                case carStyle of
                    Sedan ->
                        sedan bodyColor accentColor

                    Hatchback ->
                        hatchback bodyColor accentColor

                    Van ->
                        van bodyColor accentColor

                    _ ->
                        -- Impossible (not in the list)
                        testCar
            )


createCarMake :
    { style : CarStyle
    , bodyColor : Color
    , accentColor : Color
    , length : Length
    , width : Length
    , role : CarRole
    }
    -> CarMake
createCarMake { style, bodyColor, accentColor, length, width, role } =
    let
        halfLength =
            Quantity.half length

        halfWidth =
            Quantity.half width

        p1 =
            Point2d.xy
                (Quantity.negate halfLength)
                (Quantity.negate halfWidth)

        p2 =
            Point2d.xy
                halfLength
                (Quantity.negate halfWidth)

        p3 =
            Point2d.xy
                halfLength
                halfWidth

        p4 =
            Point2d.xy
                (Quantity.negate halfLength)
                halfWidth

        shape =
            Polygon2d.singleLoop [ p1, p2, p3, p4 ]
    in
    { bodyColor = bodyColor
    , accentColor = accentColor
    , length = length
    , width = width
    , shapeAtOrigin = shape
    , style = style
    , role = role
    }


carStyleToString : CarStyle -> String
carStyleToString style =
    case style of
        Sedan ->
            "CarSedan"

        Hatchback ->
            "CarHatchback"

        Van ->
            "CarVan"

        Firetruck ->
            "CarFiretruck"


sedan : Color -> Color -> CarMake
sedan bodyColor accentColor =
    createCarMake
        { style = Sedan
        , length = Length.meters 4.6
        , width = Length.meters 2.36
        , bodyColor = bodyColor
        , accentColor = accentColor
        , role = None
        }


hatchback : Color -> Color -> CarMake
hatchback bodyColor accentColor =
    createCarMake
        { style = Hatchback
        , length = Length.meters 4.3
        , width = Length.meters 2.5
        , bodyColor = bodyColor
        , accentColor = accentColor
        , role = None
        }


van : Color -> Color -> CarMake
van bodyColor accentColor =
    createCarMake
        { style = Van
        , length = Length.meters 5
        , width = Length.meters 2.7
        , bodyColor = bodyColor
        , accentColor = accentColor
        , role = None
        }


fireTruck : CarMake
fireTruck =
    createCarMake
        { style = Firetruck
        , length = Length.meters 6
        , width = Length.meters 2.8
        , bodyColor = Colors.red
        , accentColor = Colors.redDarker
        , role = ServiceVehicle
        }
