module Model.LocalPath exposing (..)

import Model.Geometry exposing (LMPoint2d)



-- Local path is a list of points, often constructed from a spline. Practically same as a Polyline2d.


type alias LocalPath =
    List LMPoint2d
