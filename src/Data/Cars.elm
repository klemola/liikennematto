module Data.Cars exposing
    ( CarMake
    , CarStyle
    , Shape
    , carAsset
    , sedan
    , testCar
    )

import Color exposing (Color)
import Data.Colors as Colors
import Length exposing (Length, Meters)
import Model.Geometry exposing (LMEntityCoordinates)
import Point2d
import Polygon2d exposing (Polygon2d)
import Quantity
import Svg exposing (Svg, path)
import Svg.Attributes as Attributes


type alias CarMake =
    { bodyColor : Color
    , length : Length
    , width : Length
    , shapeAtOrigin : Shape
    , style : CarStyle
    }


type CarStyle
    = Sedan


type alias Shape =
    Polygon2d Meters LMEntityCoordinates


carAsset : CarMake -> ( List (Svg msg), String )
carAsset make =
    case make.style of
        Sedan ->
            ( sedanGraphics, sedanViewBox )


sedan : CarMake
sedan =
    let
        length =
            Length.meters 4.6

        width =
            Length.meters 2.3

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
    { bodyColor = Colors.red
    , length = length
    , width = width
    , shapeAtOrigin = shape
    , style = Sedan
    }


testCar : CarMake
testCar =
    sedan


sedanViewBox : String
sedanViewBox =
    "0 0 91 49"


sedanGraphics : List (Svg msg)
sedanGraphics =
    [ path
        [ sedan.bodyColor
            |> Color.toCssString
            |> Attributes.fill
        , Attributes.d "M73.968 48.14c5.303 0 9.097-.8 11.383-2.4v-.069c2.148-1.462 3.406-3.383 3.771-5.76v-.137l.412-3.497.068-.206.206-2.537.411-9.325-.411-9.258v-.068l-.206-2.537-.068-.137-.412-3.566v-.069c-.365-2.423-1.623-4.365-3.771-5.828C83.065 1.1 79.27.277 73.968.277l-2.674.069h-.069l-8.845.48c-13.486.594-26.652.594-39.498 0h-.274l-2.743.068c-12.434.503-18.674 5.28-18.72 14.332l-.274 8.982.274 9.12c.092 8.96 6.332 13.692 18.72 14.195l2.743.068h.274c12.846-.594 26.012-.594 39.498 0l8.845.48h.069l2.674.069z"
        ]
        []
    , path
        [ Attributes.fill Colors.gray6CSS
        , Attributes.d "M83.842 43.408c-.823-5.44.252-8.822 3.223-10.148l-.206 2.674-.411 3.497c-.274 1.646-1.143 2.972-2.606 3.977z"
        ]
        []
    , path
        [ Attributes.fill Colors.gray6CSS
        , Attributes.d "M83.842 5.008c1.463 1.006 2.332 2.332 2.606 3.978l.412 3.497.205 2.674c-2.971-1.326-4.046-4.709-3.223-10.149z"
        ]
        []
    , path
        [ Attributes.fill Colors.gray6CSS
        , Attributes.d "M61.763 39.843l-.412.137h-.206l-6.034-.137.343-1.577c.914-4.48 1.417-9.166 1.509-14.057-.092-4.892-.595-9.577-1.509-14.058l-.343-1.577 6.035-.137h.205l.412.137.137.549v.137c1.371 5.029 2.057 10.011 2.057 14.949 0 4.937-.686 9.92-2.057 14.948v.137l-.137.549z"
        ]
        []
    , path
        [ Attributes.fill Colors.gray6CSS
        , Attributes.d "M29.877 9.946l-.412 1.577c-.96 4.023-1.417 8.251-1.37 12.686-.047 4.434.41 8.662 1.37 12.685l.412 1.577 6.171-.136c.412-.046.617-.275.617-.686l-1.302-13.44 1.303-13.44c0-.412-.206-.64-.618-.686l-6.171-.137z"
        ]
        []
    , path
        [ Attributes.fill Colors.gray5CSS
        , Attributes.d "M55.111 39.843l-6.651-.206c-.458-.046-.686-.297-.686-.754l1.303-14.675c0-4.8-.435-9.691-1.303-14.674 0-.457.228-.708.685-.754l6.652-.206.343 1.577c.914 4.48 1.417 9.166 1.508 14.058-.091 4.891-.594 9.577-1.508 14.057l-.343 1.577zM29.877 9.946l-.412 1.577c-.96 4.023-1.417 8.251-1.371 12.686-.046 4.434.411 8.662 1.371 12.685l.412 1.577-5.555.206-.342-.96-.892-3.223-1.371-10.285c.045-3.292.503-6.72 1.371-10.286l.892-3.223.342-.96 5.555.206z"
        ]
        []
    , path
        [ Attributes.fill Colors.gray1CSS
        , Attributes.d "M85.35 45.74c-2.285 1.6-6.08 2.4-11.382 2.4l-2.674-.069h-.069l-8.845-.48c-13.486-.594-26.652-.594-39.498 0h-.274l-2.743-.068C7.477 47.02 1.237 42.288 1.145 33.328l-.274-9.12.274-8.982C1.191 6.174 7.431 1.397 19.865.894l2.743-.068h.274c12.846.594 26.012.594 39.498 0l8.845-.48h.069l2.674-.069c5.303 0 9.097.823 11.383 2.469 2.148 1.463 3.406 3.405 3.771 5.828v.069l.412 3.566.068.137.206 2.537v.068l.411 9.258-.411 9.325-.206 2.537-.068.206-.412 3.497v.137c-.365 2.377-1.623 4.297-3.771 5.76v.069zm-1.508-2.332c1.463-1.005 2.332-2.33 2.606-3.977l.412-3.497.205-2.674.412-9.051-.412-9.052-.206-2.674-.41-3.497c-.275-1.646-1.144-2.972-2.607-3.977-2.011-1.326-5.302-1.989-9.874-1.989l-2.537.069-8.914.48c-13.578.594-26.835.594-39.772 0h-.137l-2.606.068C9.488 4.05 4.117 7.934 3.888 15.294l-.274 8.915.274 8.914c.229 7.36 5.6 11.245 16.114 11.657l2.606.069h.137c12.937-.595 26.195-.595 39.772 0l8.914.48 2.537.068c4.572 0 7.863-.663 9.874-1.989z"
        ]
        []
    ]
