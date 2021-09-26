module Model.Geometry exposing
    ( LMBoundingBox2d
    , LMDirection2d
    , LMEntityCoordinates
    , LMPoint2d
    , LMPolyline2d
    , down
    , left
    , pixelsToMeters
    , pixelsToMetersRatio
    , pointToPixels
    , right
    , toPixelsValue
    , up
    )

import BoundingBox2d exposing (BoundingBox2d)
import Direction2d exposing (Direction2d)
import Length exposing (Length)
import Pixels
import Point2d exposing (Point2d)
import Polyline2d exposing (Polyline2d)
import Quantity exposing (Quantity, Rate)


type alias LMEntityCoordinates =
    ()


type alias LMPoint2d =
    Point2d Length.Meters LMEntityCoordinates


type alias LMDirection2d =
    Direction2d LMEntityCoordinates


type alias LMBoundingBox2d =
    BoundingBox2d Length.Meters LMEntityCoordinates


type alias LMPolyline2d =
    Polyline2d Length.Meters LMEntityCoordinates



--
-- Pixels conversion
--


pixelsToMetersRatio : Quantity Float (Rate Pixels.Pixels Length.Meters)
pixelsToMetersRatio =
    Pixels.pixels 5 |> Quantity.per (Length.meters 1)


pixelsToMeters : Float -> Length
pixelsToMeters pixels =
    Pixels.float pixels
        |> Quantity.at_ pixelsToMetersRatio


toPixelsValue : Length -> Float
toPixelsValue length =
    length
        |> Quantity.at pixelsToMetersRatio
        |> Pixels.inPixels


pointToPixels : LMPoint2d -> { x : Float, y : Float }
pointToPixels point =
    point
        |> Point2d.at pixelsToMetersRatio
        |> Point2d.toPixels



--
-- Alias
--


up : Direction2d.Direction2d LMEntityCoordinates
up =
    Direction2d.positiveY


right : Direction2d.Direction2d LMEntityCoordinates
right =
    Direction2d.positiveX


down : Direction2d.Direction2d LMEntityCoordinates
down =
    Direction2d.negativeY


left : Direction2d.Direction2d LMEntityCoordinates
left =
    Direction2d.negativeX
