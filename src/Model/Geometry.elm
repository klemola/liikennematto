module Model.Geometry exposing
    ( LMBoundingBox2d
    , LMDirection2d
    , LMEntityCoordinates
    , LMPoint2d
    )

import BoundingBox2d exposing (BoundingBox2d)
import Direction2d exposing (Direction2d)
import Length
import Point2d exposing (Point2d)


type alias LMEntityCoordinates =
    ()


type alias LMPoint2d =
    Point2d Length.Meters LMEntityCoordinates


type alias LMDirection2d =
    Direction2d LMEntityCoordinates


type alias LMBoundingBox2d =
    BoundingBox2d Length.Meters LMEntityCoordinates
