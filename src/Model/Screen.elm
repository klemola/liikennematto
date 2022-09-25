module Model.Screen exposing
    ( Screen
    , ScreenCategory(..)
    , fallback
    , fromDimensions
    )

import Element


type alias Screen =
    { width : Int
    , height : Int
    , orientation : Element.Orientation
    , category : ScreenCategory
    }


type ScreenCategory
    = SizeSM
    | SizeMD
    | SizeLG
    | SizeXL
    | SizeXXL


fallback : Screen
fallback =
    fromDimensions 375 667


fromDimensions : Int -> Int -> Screen
fromDimensions width height =
    { width = width
    , height = height
    , orientation =
        if width < height then
            Element.Portrait

        else
            Element.Landscape
    , category = screenCategory width height
    }


breakpointMD : Int
breakpointMD =
    768


breakpointLG : Int
breakpointLG =
    992


breakpointXL : Int
breakpointXL =
    1200


breakpointXXL : Int
breakpointXXL =
    1400


screenCategory : Int -> Int -> ScreenCategory
screenCategory width height =
    let
        maxDimension =
            max width height
    in
    if maxDimension < breakpointMD then
        SizeSM

    else if maxDimension < breakpointLG then
        SizeMD

    else if maxDimension < breakpointXL then
        SizeLG

    else if maxDimension < breakpointXXL then
        SizeXL

    else
        SizeXXL
