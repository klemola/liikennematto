module Common exposing (..)

import Angle exposing (Angle)
import BoundingBox2d
import Direction2d
import Length exposing (Length)
import Model.Geometry exposing (LMBoundingBox2d, LMDirection2d, LMPoint2d)
import Point2d


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


boundingBoxOverlaps : LMBoundingBox2d -> LMBoundingBox2d -> Bool
boundingBoxOverlaps bb1 bb2 =
    BoundingBox2d.overlappingByAtLeast (Length.meters 0.1) bb1 bb2
