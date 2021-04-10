module Geometry exposing
    ( LMBoundingBox2d
    , LMDirection2d
    , LMEntityCoordinates
    , LMPoint2d
    , LMTriangle2d
    , accelerationToString
    , angleFromDirection
    , angleToTarget
    , boundingBoxFromCircle
    , boundingBoxWithDimensions
    , fieldOfViewTriangle
    , noBoundingBoxOverlap
    , pointToString
    , speedToString
    )

import Acceleration exposing (Acceleration)
import Angle exposing (Angle)
import BoundingBox2d exposing (BoundingBox2d)
import Circle2d
import Config
    exposing
        ( pixelsToMetersRatio
        )
import Direction2d exposing (Direction2d)
import Length exposing (Length, Meters)
import Point2d exposing (Point2d)
import Quantity
import Speed exposing (Speed)
import Triangle2d exposing (Triangle2d)


type alias LMEntityCoordinates =
    ()


type alias LMPoint2d =
    Point2d Meters LMEntityCoordinates


type alias LMDirection2d =
    Direction2d LMEntityCoordinates


type alias LMBoundingBox2d =
    BoundingBox2d Meters LMEntityCoordinates


type alias LMTriangle2d =
    Triangle2d Meters LMEntityCoordinates



-- Conversion


speedToString : Speed -> String
speedToString speed =
    let
        speedValue =
            speed
                |> Quantity.unwrap
                |> String.fromFloat
    in
    "Speed: " ++ speedValue ++ " m/s"


accelerationToString : Acceleration -> String
accelerationToString acceleration =
    let
        accelerationValue =
            acceleration
                |> Quantity.unwrap
                |> String.fromFloat
    in
    "Acceleration: " ++ accelerationValue ++ " m/s²"


pointToString : LMPoint2d -> String
pointToString point =
    let
        { x, y } =
            point
                |> Point2d.at pixelsToMetersRatio
                |> Point2d.toPixels

        format n =
            n
                |> truncate
                |> String.fromInt
                |> String.padLeft 2 ' '
    in
    String.join
        " "
        [ "x:"
        , format x
        , "y:"
        , format y
        ]



-- Angles


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



-- Bounding boxes


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



-- Triangles and field of view


fieldOfViewTriangle : LMPoint2d -> LMDirection2d -> Angle -> Length -> LMTriangle2d
fieldOfViewTriangle origin direction fov distance =
    let
        leftVertexDirection =
            Direction2d.rotateBy fov direction

        rightVertexAngle =
            Quantity.minus fov (Angle.degrees 360)

        rightVertexDirection =
            Direction2d.rotateBy rightVertexAngle direction

        farLeftVertex =
            origin |> Point2d.translateIn leftVertexDirection distance

        farRightVertex =
            origin |> Point2d.translateIn rightVertexDirection distance
    in
    Triangle2d.fromVertices ( origin, farLeftVertex, farRightVertex )
