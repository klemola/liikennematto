module Collision exposing (BoundingBox, aabb)


type alias BoundingBox =
    { x : Float
    , y : Float
    , width : Float
    , height : Float
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
