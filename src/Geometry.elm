module Geometry exposing
    ( LMBoundingBox2d
    , LMDirection2d
    , LMEntityCoordinates
    , LMPoint2d
    , angleFromDirection
    , angleToTarget
    , boundingBoxFromCircle
    , boundingBoxWithDimensions
    , noBoundingBoxOverlap
    )

import Angle exposing (Angle)
import BoundingBox2d exposing (BoundingBox2d)
import Circle2d
import Direction2d exposing (Direction2d)
import Length exposing (Length, Meters)
import Point2d exposing (Point2d)


type alias LMEntityCoordinates =
    ()


type alias LMPoint2d =
    Point2d Meters LMEntityCoordinates


type alias LMDirection2d =
    Direction2d LMEntityCoordinates


type alias LMBoundingBox2d =
    BoundingBox2d Meters LMEntityCoordinates


angleToTarget : LMPoint2d -> LMPoint2d -> Angle
angleToTarget target origin =
    Direction2d.from origin target
        |> Maybe.map Direction2d.toAngle
        |> Maybe.withDefault (Angle.degrees 0)


angleFromDirection : LMDirection2d -> LMPoint2d -> LMPoint2d -> Angle
angleFromDirection direction target origin =
    Direction2d.from origin target
        |> Maybe.map (Direction2d.angleFrom direction)
        |> Maybe.withDefault (Angle.degrees 0)


boundingBoxWithDimensions : Length -> Length -> LMPoint2d -> LMBoundingBox2d
boundingBoxWithDimensions width height origin =
    let
        otherCorner =
            origin
                |> Point2d.translateIn Direction2d.positiveX width
                |> Point2d.translateIn Direction2d.positiveY height
    in
    BoundingBox2d.from origin otherCorner


noBoundingBoxOverlap : LMBoundingBox2d -> LMBoundingBox2d -> Bool
noBoundingBoxOverlap bb1 bb2 =
    not <| BoundingBox2d.overlappingByAtLeast (Length.meters 0.1) bb1 bb2


boundingBoxFromCircle : LMPoint2d -> Length -> LMBoundingBox2d
boundingBoxFromCircle position radius =
    Circle2d.atPoint position radius |> Circle2d.boundingBox
