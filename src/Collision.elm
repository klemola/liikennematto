module Collision exposing (BoundingBox, aabb, boundingBoxAroundCenter)

import Position exposing (Position)


type alias BoundingBox =
    { x : Float
    , y : Float
    , width : Float
    , height : Float
    }


boundingBoxAroundCenter : Position -> Float -> BoundingBox
boundingBoxAroundCenter ( x, y ) size =
    { x = x - size / 2
    , y = y - size / 2
    , width = size
    , height = size
    }



{-
   Axis-aligned bounding box check
-}


aabb : BoundingBox -> BoundingBox -> Bool
aabb boxA boxB =
    (boxA.x < boxB.x + boxB.width)
        && (boxA.x + boxA.width > boxB.x)
        && (boxA.y < boxB.y + boxB.height)
        && (boxA.y + boxA.height > boxB.y)
