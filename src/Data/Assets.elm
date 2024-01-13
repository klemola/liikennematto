module Data.Assets exposing
    ( Assets
    , assetById
    , innerLaneOffset
    , outerLaneOffset
    , roads
    , roadsLegacy
    )

import Data.Colors as Colors
import Dict exposing (Dict)
import Length exposing (Length)
import Svg exposing (Svg, path)
import Svg.Attributes as Attributes


type alias Assets msg =
    Dict Int (List (Svg msg))


assetById : Assets msg -> Int -> List (Svg msg)
assetById assets assetId =
    case Dict.get assetId assets of
        Just asset ->
            asset

        Nothing ->
            []


innerLaneOffset : Length
innerLaneOffset =
    -- the distance from a road tile's edge to the inner lane (from left / bottom side)
    Length.meters 6


outerLaneOffset : Length
outerLaneOffset =
    -- the distance from a road tile's edge to the outer lane (from the left / bottom side)
    Length.meters 10


roads : Assets ()
roads =
    Dict.fromList
        [ ( 17, defaultRoad )
        , ( 1, deadendDown )
        , ( 2, deadendRight )
        , ( 3, curveBottomRight )
        , ( 4, deadendLeft )
        , ( 5, curveBottomLeft )
        , ( 6, regularHorizontal )
        , ( 7, intersectionTUp )
        , ( 8, deadendUp )
        , ( 9, regularVertical )
        , ( 10, curveTopRight )
        , ( 11, intersectionTLeft )
        , ( 12, curveTopLeft )
        , ( 13, intersectionTRight )
        , ( 14, intersectionTDown )
        , ( 15, intersectionCrossroads )
        , ( 20, lotEntryTUp )
        , ( 21, lotEntryTRight )
        , ( 22, lotEntryTLeft )
        , ( 30, lotDebugCornerTopLeft )
        , ( 31, lotDebugCornerTopRight )
        , ( 32, lotDebugCornerBottomRight )
        , ( 33, lotDebugCornerBottomLeft )
        , ( 34, lotDebugCenter )
        , ( 35, lotDebugCenter )
        , ( 36, lotDebugCenter )
        , ( 37, lotDebugCenter )
        , ( 38, lotDebugCenter )
        , ( 40, lotDebugDrivewayRight )
        , ( 41, lotDebugDrivewayLeft )
        , ( 42, lotDebugDrivewayUp )
        ]


roadsLegacy : Assets ()
roadsLegacy =
    Dict.fromList
        [ ( 0, defaultRoad )
        , ( 1, deadendDown )
        , ( 2, deadendRight )
        , ( 3, curveBottomRight )
        , ( 4, deadendLeft )
        , ( 5, curveBottomLeft )
        , ( 6, regularHorizontal )
        , ( 7, intersectionTUp )
        , ( 8, deadendUp )
        , ( 9, regularVertical )
        , ( 10, curveTopRight )
        , ( 11, intersectionTLeft )
        , ( 12, curveTopLeft )
        , ( 13, intersectionTRight )
        , ( 14, intersectionTDown )
        , ( 15, intersectionCrossroads )
        , ( 23, lotEntryTUp )
        , ( 27, lotEntryTLeft )
        , ( 29, lotEntryTRight )
        ]


defaultRoad : List (Svg msg)
defaultRoad =
    [ Svg.circle
        [ Attributes.cx "128"
        , Attributes.cy "128"
        , Attributes.r "98"
        , Attributes.fill Colors.gray3CSS
        , Attributes.stroke Colors.gray1CSS
        , Attributes.strokeWidth "4"
        ]
        []
    , Svg.circle
        [ Attributes.cx "128"
        , Attributes.cy "128"
        , Attributes.r "60"
        , Attributes.fill Colors.gray2CSS
        ]
        []
    ]


deadendDown : List (Svg msg)
deadendDown =
    [ path
        [ Attributes.fill Colors.gray3CSS
        , Attributes.d "M32 128V0h192v128a95.996 95.996 0 01-28.118 67.882 95.996 95.996 0 01-135.764 0A95.998 95.998 0 0132 128z"
        ]
        []
    , path
        [ Attributes.fill Colors.gray2CSS
        , Attributes.d "M188 0H68v128c0 33.137 26.863 60 60 60s60-26.863 60-60V0z"
        ]
        []
    , path
        [ Attributes.fill Colors.gray6CSS
        , Attributes.d "M126 13v25.6h4V13h-4zm0 51.2v25.6h4V64.2h-4z"
        ]
        []
    , path
        [ Attributes.stroke Colors.gray1CSS
        , Attributes.strokeWidth "3"
        , Attributes.fill "none"
        , Attributes.d "M30.5 0v126.867c0 56.513 45.04 98.633 97.5 98.633 52.459 0 97.5-42.119 97.5-98.633V0"
        ]
        []
    ]


deadendRight : List (Svg msg)
deadendRight =
    [ path
        [ Attributes.fill Colors.gray3CSS
        , Attributes.d "M128 224H0V32h128a95.998 95.998 0 0196 96 95.996 95.996 0 01-28.118 67.882A95.996 95.996 0 01128 224z"
        ]
        []
    , path
        [ Attributes.fill Colors.gray2CSS
        , Attributes.d "M0 68v120h128c33.137 0 60-26.863 60-60s-26.863-60-60-60H0z"
        ]
        []
    , path
        [ Attributes.fill Colors.gray6CSS
        , Attributes.d "M13 130h25.6v-4H13v4zm51.2 0h25.6v-4H64.2v4z"
        ]
        []
    , path
        [ Attributes.stroke Colors.gray1CSS
        , Attributes.strokeWidth "3"
        , Attributes.fill "none"
        , Attributes.d "M0 225.5h126.867c56.513 0 98.633-45.041 98.633-97.5 0-52.46-42.119-97.5-98.633-97.5H0"
        ]
        []
    ]


curveBottomRight : List (Svg msg)
curveBottomRight =
    [ path
        [ Attributes.fill Colors.gray3CSS
        , Attributes.d "M121.835 188C86.767 210.773 44.928 224 0 224V32c17.673 0 32-14.327 32-32h192c0 78.713-40.6 147.944-102 187.893V188h-.165z"
        ]
        []
    , path
        [ Attributes.fill Colors.gray2CSS
        , Attributes.d "M0 188c103.83 0 188-84.17 188-188H68c0 37.555-30.445 68-68 68v120z"
        ]
        []
    , path
        [ Attributes.fill Colors.gray1CSS
        , Attributes.d "M0 32c17.673 0 32-14.327 32-32h-3c0 16.016-12.984 29-29 29v3zm0 195c125.369 0 227-101.631 227-227h-3c0 123.712-100.288 224-224 224v3z"
        ]
        []
    , path
        [ Attributes.fill Colors.gray6CSS
        , Attributes.d "M12.31 125.399l.396 3.98a131.715 131.715 0 0025.006-4.967l-1.156-3.83a127.688 127.688 0 01-12.007 2.995 127.496 127.496 0 01-12.239 1.822zM59.387 111.157l1.883 3.529a129.19 129.19 0 0021.19-14.162l-2.54-3.09a125.228 125.228 0 01-20.533 13.723zM97.378 79.954l3.093 2.537a130.779 130.779 0 0014.166-21.19l-3.527-1.888a126.767 126.767 0 01-13.732 20.541zM120.589 36.6l3.826 1.165a129.196 129.196 0 004.987-24.993l-3.981-.392a125.208 125.208 0 01-4.832 24.22z"
        ]
        []
    ]


deadendLeft : List (Svg msg)
deadendLeft =
    [ path
        [ Attributes.fill Colors.gray3CSS
        , Attributes.d "M128 32h128v192H128a95.998 95.998 0 01-96-96 96 96 0 0196-96z"
        ]
        []
    , path
        [ Attributes.fill Colors.gray2CSS
        , Attributes.d "M256 188V68H128c-33.137 0-60 26.863-60 60s26.863 60 60 60h128z"
        ]
        []
    , path
        [ Attributes.fill Colors.gray6CSS
        , Attributes.d "M243 126h-25.6v4H243v-4zm-51.2 0h-25.6v4h25.6v-4z"
        ]
        []
    , path
        [ Attributes.stroke Colors.gray1CSS
        , Attributes.strokeWidth "3"
        , Attributes.fill "none"
        , Attributes.d "M256 30.5H129.133C72.62 30.5 30.5 75.54 30.5 128c0 52.459 42.12 97.5 98.633 97.5H256"
        ]
        []
    ]


curveBottomLeft : List (Svg msg)
curveBottomLeft =
    [ path
        [ Attributes.fill Colors.gray3CSS
        , Attributes.d "M68 121.835C45.227 86.767 32 44.928 32 0h192c0 17.673 14.327 32 32 32v192c-78.713 0-147.944-40.6-187.893-102H68v-.165z"
        ]
        []
    , path
        [ Attributes.fill Colors.gray2CSS
        , Attributes.d "M68 0c0 103.83 84.17 188 188 188V68c-37.555 0-68-30.445-68-68H68z"
        ]
        []
    , path
        [ Attributes.fill Colors.gray1CSS
        , Attributes.d "M224 0c0 17.673 14.327 32 32 32v-3c-16.016 0-29-12.984-29-29h-3zM29 0c0 125.369 101.631 227 227 227v-3C132.288 224 32 123.712 32 0h-3z"
        ]
        []
    , path
        [ Attributes.fill Colors.gray6CSS
        , Attributes.d "M130.601 12.31l-3.98.396a131.715 131.715 0 004.967 25.006l3.83-1.156a127.38 127.38 0 01-2.995-12.007 127.496 127.496 0 01-1.822-12.239zM144.843 59.387l-3.529 1.883a129.19 129.19 0 0014.162 21.19l3.09-2.54a125.257 125.257 0 01-13.723-20.533zM176.046 97.378l-2.537 3.093a130.79 130.79 0 0021.189 14.166l1.889-3.527a126.732 126.732 0 01-20.541-13.732zM219.4 120.589l-1.165 3.826a129.196 129.196 0 0024.993 4.987l.392-3.981a125.215 125.215 0 01-24.22-4.832z"
        ]
        []
    ]


regularHorizontal : List (Svg msg)
regularHorizontal =
    [ Svg.rect
        [ Attributes.y "224"
        , Attributes.width "192"
        , Attributes.height "256"
        , Attributes.transform "rotate(-90 0 224)"
        , Attributes.fill "#BCA9A9"
        ]
        []
    , Svg.rect
        [ Attributes.y "188"
        , Attributes.width "120"
        , Attributes.height "256"
        , Attributes.transform "rotate(-90 0 188)"
        , Attributes.fill "#766565"
        ]
        []
    , path
        [ Attributes.d "M13 128H38.6M64.2 128H89.8M115.4 128H141M166.6 128H192.2M217.8 128H243.4"
        , Attributes.stroke "#F0F0DD"
        , Attributes.strokeWidth "4"
        , Attributes.fill "none"
        ]
        []
    , path
        [ Attributes.d "M0 30.5L256 30.5M0 225.5L256 225.5"
        , Attributes.stroke "#3D3434"
        , Attributes.strokeWidth "3"
        , Attributes.fill "none"
        ]
        []
    ]


intersectionTUp : List (Svg msg)
intersectionTUp =
    [ path
        [ Attributes.d "M256 32C238.327 32 224 17.6731 224 0H32C32 17.6731 17.6731 32 0 32V224H256V32Z"
        , Attributes.fill "#BCA9A9"
        ]
        []
    , path
        [ Attributes.d "M68 0H188V36C188 53.6731 202.327 68 220 68H256V188H188H68H0V68H36C53.6731 68 68 53.6731 68 36V0Z"
        , Attributes.fill "#766565"
        ]
        []
    , path
        [ Attributes.d "M128 6V38M106 6V38M84 6V38M150 6V38M172 6V38M38 128H6M38 106H6M38 84H6M38 150H6M38 172H6"
        , Attributes.stroke "#F0F0DD"
        , Attributes.strokeWidth "6"
        , Attributes.fill "none"
        ]
        []
    , path
        [ Attributes.d "M30.5 0C30.5 16.8447 16.8447 30.5 0 30.5M256 30.5C239.155 30.5 225.5 16.8447 225.5 0M256 225.5H0"
        , Attributes.stroke "#3D3434"
        , Attributes.strokeWidth "3"
        , Attributes.fill "none"
        ]
        []
    ]


deadendUp : List (Svg msg)
deadendUp =
    [ path
        [ Attributes.fill Colors.gray3CSS
        , Attributes.d "M224 128v128H32V128a96 96 0 11192 0z"
        ]
        []
    , path
        [ Attributes.fill Colors.gray2CSS
        , Attributes.d "M68 256h120V128c0-33.137-26.863-60-60-60s-60 26.863-60 60v128z"
        ]
        []
    , path
        [ Attributes.fill Colors.gray6CSS
        , Attributes.d "M130 243v-25.6h-4V243h4zm0-51.2v-25.6h-4v25.6h4z"
        ]
        []
    , path
        [ Attributes.stroke Colors.gray1CSS
        , Attributes.strokeWidth "3"
        , Attributes.fill "none"
        , Attributes.d "M225.5 256V129.133C225.5 72.62 180.459 30.5 128 30.5c-52.46 0-97.5 42.12-97.5 98.633V256"
        ]
        []
    ]


regularVertical : List (Svg msg)
regularVertical =
    [ Svg.rect
        [ Attributes.x "32"
        , Attributes.width "192"
        , Attributes.height "256"
        , Attributes.fill "#BCA9A9"
        ]
        []
    , Svg.rect
        [ Attributes.x "68"
        , Attributes.width "120"
        , Attributes.height "256"
        , Attributes.fill "#766565"
        ]
        []
    , path
        [ Attributes.d "M128 13V38.6M128 64.2V89.8M128 115.4V141M128 166.6V192.2M128 217.8V243.4"
        , Attributes.stroke "#F0F0DD"
        , Attributes.strokeWidth "4"
        , Attributes.fill "none"
        ]
        []
    , path
        [ Attributes.d "M225.5 0V256M30.5 0V256"
        , Attributes.stroke "#3D3434"
        , Attributes.strokeWidth "3"
        , Attributes.fill "none"
        ]
        []
    ]


curveTopRight : List (Svg msg)
curveTopRight =
    [ path
        [ Attributes.fill Colors.gray3CSS
        , Attributes.d "M188 134.165c22.773 35.068 36 76.907 36 121.835H32c0-17.673-14.327-32-32-32V32c78.713 0 147.944 40.6 187.893 102H188v.165z"
        ]
        []
    , path
        [ Attributes.fill Colors.gray2CSS
        , Attributes.d "M188 256C188 152.17 103.83 68 0 68v120c37.555 0 68 30.445 68 68h120z"
        ]
        []
    , path
        [ Attributes.fill Colors.gray1CSS
        , Attributes.d "M32 256c0-17.673-14.327-32-32-32v3c16.016 0 29 12.984 29 29h3zm195 0C227 130.631 125.369 29 0 29v3c123.712 0 224 100.288 224 224h3z"
        ]
        []
    , path
        [ Attributes.fill Colors.gray6CSS
        , Attributes.d "M125.399 243.69l3.98-.396a131.749 131.749 0 00-4.967-25.005l-3.83 1.155a127.411 127.411 0 012.995 12.007 127.438 127.438 0 011.822 12.239zM111.157 196.613l3.529-1.883a129.17 129.17 0 00-14.162-21.19l-3.09 2.54a125.252 125.252 0 0113.723 20.533zM79.954 158.622l2.537-3.093a130.774 130.774 0 00-21.19-14.166l-1.888 3.527a126.755 126.755 0 0120.541 13.732zM36.6 135.411l1.165-3.826a129.196 129.196 0 00-24.993-4.987l-.392 3.981a125.208 125.208 0 0124.22 4.832z"
        ]
        []
    ]


intersectionTLeft : List (Svg msg)
intersectionTLeft =
    [ path
        [ Attributes.d "M32 256C32 238.327 17.6731 224 0 224L0 32C17.6731 32 32 17.6731 32 0H224L224 256H32Z"
        , Attributes.fill "#BCA9A9"
        ]
        []
    , path
        [ Attributes.d "M0 68L0 188H36C53.6731 188 68 202.327 68 220V256H188V188V68L188 0H68V36C68 53.6731 53.6731 68 36 68H0Z"
        , Attributes.fill "#766565"
        ]
        []
    , path
        [ Attributes.d "M6 128H38M6 106H38M6 84H38M6 150H38M6 172H38M128 38V6M106 38V6M84 38V6M150 38V6M172 38V6"
        , Attributes.stroke "#F0F0DD"
        , Attributes.strokeWidth "6"
        , Attributes.fill "none"
        ]
        []
    , path
        [ Attributes.d "M0 30.5C16.8447 30.5 30.5 16.8447 30.5 0M30.5 256C30.5 239.155 16.8447 225.5 0 225.5M225.5 256L225.5 0"
        , Attributes.stroke "#3D3434"
        , Attributes.strokeWidth "3"
        , Attributes.fill "none"
        ]
        []
    ]


curveTopLeft : List (Svg msg)
curveTopLeft =
    [ path
        [ Attributes.fill Colors.gray3CSS
        , Attributes.d "M68 134.165C45.227 169.233 32 211.072 32 256h192c0-17.673 14.327-32 32-32V32c-78.713 0-147.944 40.6-187.893 102H68v.165z"
        ]
        []
    , path
        [ Attributes.fill Colors.gray2CSS
        , Attributes.d "M68 256c0-103.83 84.17-188 188-188v120c-37.555 0-68 30.445-68 68H68z"
        ]
        []
    , path
        [ Attributes.fill Colors.gray1CSS
        , Attributes.d "M224 256c0-17.673 14.327-32 32-32v3c-16.016 0-29 12.984-29 29h-3zm-195 0C29 130.631 130.631 29 256 29v3C132.288 32 32 132.288 32 256h-3z"
        ]
        []
    , path
        [ Attributes.fill Colors.gray6CSS
        , Attributes.d "M130.601 243.69l-3.98-.396a131.749 131.749 0 014.967-25.005l3.83 1.155a127.411 127.411 0 00-2.995 12.007 127.438 127.438 0 00-1.822 12.239zM144.843 196.613l-3.529-1.883a129.17 129.17 0 0114.162-21.19l3.09 2.54a125.276 125.276 0 00-13.723 20.533zM176.046 158.622l-2.537-3.093a130.79 130.79 0 0121.189-14.166l1.889 3.527a126.72 126.72 0 00-20.541 13.732zM219.4 135.411l-1.165-3.826a129.196 129.196 0 0124.993-4.987l.392 3.981a125.215 125.215 0 00-24.22 4.832z"
        ]
        []
    ]


intersectionTRight : List (Svg msg)
intersectionTRight =
    [ path
        [ Attributes.d "M224 256C224 238.327 238.327 224 256 224V32C238.327 32 224 17.6731 224 0H32L32 256H224Z"
        , Attributes.fill "#BCA9A9"
        ]
        []
    , path
        [ Attributes.d "M256 68V188H220C202.327 188 188 202.327 188 220V256H68V188V68L68 0H188V36C188 53.6731 202.327 68 220 68H256Z"
        , Attributes.fill "#766565"
        ]
        []
    , path
        [ Attributes.d "M250 128H218M250 106H218M250 84H218M250 150H218M250 172H218M128 38V6M150 38V6M172 38V6M106 38V6M84 38V6"
        , Attributes.stroke "#F0F0DD"
        , Attributes.strokeWidth "6"
        , Attributes.fill "none"
        ]
        []
    , path
        [ Attributes.d "M256 30.5C239.155 30.5 225.5 16.8447 225.5 0M225.5 256C225.5 239.155 239.155 225.5 256 225.5M30.5 256L30.5 0"
        , Attributes.stroke "#3D3434"
        , Attributes.strokeWidth "3"
        , Attributes.fill "none"
        ]
        []
    ]


intersectionTDown : List (Svg msg)
intersectionTDown =
    [ path
        [ Attributes.d "M256 224C238.327 224 224 238.327 224 256L32 256C32 238.327 17.6731 224 0 224L0 32L256 32V224Z"
        , Attributes.fill "#BCA9A9"
        ]
        []
    , path
        [ Attributes.d "M68 256L188 256V220C188 202.327 202.327 188 220 188H256V68H188L68 68H0L0 188H36C53.6731 188 68 202.327 68 220V256Z"
        , Attributes.fill "#766565"
        ]
        []
    , path
        [ Attributes.d "M128 250V218M106 250V218M84 250V218M150 250V218M172 250V218M38 128H6M38 150H6M38 172H6M38 106H6M38 84H6"
        , Attributes.stroke "#F0F0DD"
        , Attributes.strokeWidth "6"
        , Attributes.fill "none"
        ]
        []
    , path
        [ Attributes.d "M30.5 256C30.5 239.155 16.8447 225.5 0 225.5M256 225.5C239.155 225.5 225.5 239.155 225.5 256M256 30.5L0 30.5"
        , Attributes.stroke "#3D3434"
        , Attributes.strokeWidth "3"
        , Attributes.fill "none"
        ]
        []
    ]


intersectionCrossroads : List (Svg msg)
intersectionCrossroads =
    [ path
        [ Attributes.fill Colors.gray3CSS
        , Attributes.d "M224 0c0 17.673 14.327 32 32 32v192c-17.673 0-32 14.327-32 32H32c0-17.673-14.327-32-32-32V32c17.673 0 32-14.327 32-32h192z"
        ]
        []
    , path
        [ Attributes.fill Colors.gray2CSS
        , Attributes.d "M188 0H68v36c0 17.673-14.327 32-32 32H0v120h36c17.673 0 32 14.327 32 32v36h120v-36c0-17.673 14.327-32 32-32h36V68h-36c-17.673 0-32-14.327-32-32V0z"
        ]
        []
    , path
        [ Attributes.fill Colors.gray6CSS
        , Attributes.d "M125 6v32h6V6h-6zm22 0v32h6V6h-6zm22 0v32h6V6h-6zm-66 0v32h6V6h-6zM81 6v32h6V6h-6zm44 212v32h6v-32h-6zm22 0v32h6v-32h-6zm22 0v32h6v-32h-6zm-66 0v32h6v-32h-6zm-22 0v32h6v-32h-6zm137-87h32v-6h-32v6zm0-22h32v-6h-32v6zm0-22h32v-6h-32v6zm0 66h32v-6h-32v6zm0 22h32v-6h-32v6zM6 131h32v-6H6v6zm0-22h32v-6H6v6zm0-22h32v-6H6v6zm0 66h32v-6H6v6zm0 22h32v-6H6v6z"
        ]
        []
    , path
        [ Attributes.fill Colors.gray1CSS
        , Attributes.d "M255.496 30.492l-.025 1.5.025-1.5zm-.025 1.5l.504.008.05-3-.504-.008-.05 3zM224 0c0 17.466 14.007 31.705 31.471 31.991l.05-3C239.694 28.734 227 15.83 227 0h-3zM32 256c0-17.673-14.327-32-32-32v3c16.016 0 29 12.984 29 29h3zm224-32c-17.673 0-32 14.327-32 32h3c0-16.016 12.984-29 29-29v-3zM0 32c17.673 0 32-14.327 32-32h-3c0 16.016-12.984 29-29 29v3z"
        ]
        []
    ]



--
-- Lot modifiers
--


lotEntryTUp : List (Svg msg)
lotEntryTUp =
    [ path
        [ Attributes.d "M136 0V16C136 24.8366 143.163 32 152 32H256V224H0V32C8.83656 32 16 24.8366 16 16V0H136Z"
        , Attributes.fill Colors.gray3CSS
        ]
        []
    , path
        [ Attributes.d "M0 68V188H256V68H0Z"
        , Attributes.fill Colors.gray2CSS
        ]
        []
    , path
        [ Attributes.d "M0 227H256V224H0V227ZM136 0V16H139V0H136ZM152 32H256V29H152V32ZM136 16C136 24.8366 143.163 32 152 32V29C144.82 29 139 23.1797 139 16H136ZM16 16V0H13V16H16ZM13 16C13 23.1797 7.1797 29 0 29V32C8.83656 32 16 24.8366 16 16H13Z"
        , Attributes.fill Colors.gray1CSS
        ]
        []
    , path
        [ Attributes.d "M12.8 130.2H38.4V126.2H12.8V130.2ZM64.0001 130.2H89.6001V126.2H64.0001V130.2ZM115.2 130.2H140.8V126.2H115.2V130.2ZM166.4 130.2H192V126.2H166.4V130.2ZM217.6 130.2H243.2V126.2H217.6V130.2Z"
        , Attributes.fill Colors.gray6CSS
        ]
        []
    ]


lotEntryTLeft : List (Svg msg)
lotEntryTLeft =
    [ path
        [ Attributes.d "M0 120H16C24.8366 120 32 112.837 32 104L32 0H224L224 256H32C32 247.163 24.8366 240 16 240H0L0 120Z"
        , Attributes.fill Colors.gray3CSS
        ]
        []
    , path
        [ Attributes.d "M68 256H188V0H68L68 256Z"
        , Attributes.fill Colors.gray2CSS
        ]
        []
    , path
        [ Attributes.d "M227 256L227 0H224L224 256H227ZM0 120H16V117H0L0 120ZM32 104L32 0H29L29 104H32ZM16 120C24.8366 120 32 112.837 32 104H29C29 111.18 23.1797 117 16 117V120ZM16 240H0L0 243H16V240ZM16 243C23.1797 243 29 248.82 29 256H32C32 247.163 24.8366 240 16 240V243Z"
        , Attributes.fill Colors.gray1CSS
        ]
        []
    , path
        [ Attributes.d "M130.2 243.2V217.6H126.2V243.2H130.2ZM130.2 192V166.4H126.2V192H130.2ZM130.2 140.8V115.2H126.2L126.2 140.8H130.2ZM130.2 89.5999V63.9999H126.2V89.5999H130.2ZM130.2 38.3999V12.7999H126.2V38.3999H130.2Z"
        , Attributes.fill Colors.gray6CSS
        ]
        []
    ]


lotEntryTRight : List (Svg msg)
lotEntryTRight =
    [ path
        [ Attributes.d "M256 120H240C231.163 120 224 112.837 224 104V0H32L32 256H224C224 247.163 231.163 240 240 240H256V120Z"
        , Attributes.fill Colors.gray3CSS
        ]
        []
    , path
        [ Attributes.d "M188 256H68L68 0H188L188 256Z"
        , Attributes.fill Colors.gray2CSS
        ]
        []
    , path
        [ Attributes.d "M29 256L29 0H32L32 256H29ZM256 120H240V117H256V120ZM224 104V0H227V104H224ZM240 120C231.163 120 224 112.837 224 104H227C227 111.18 232.82 117 240 117V120ZM240 240H256V243H240V240ZM240 243C232.82 243 227 248.82 227 256H224C224 247.163 231.163 240 240 240V243Z"
        , Attributes.fill Colors.gray1CSS
        ]
        []
    , path
        [ Attributes.d "M125.8 243.2V217.6H129.8V243.2H125.8ZM125.8 192V166.4H129.8V192H125.8ZM125.8 140.8L125.8 115.2H129.8V140.8H125.8ZM125.8 89.5999V63.9999H129.8V89.5999H125.8ZM125.8 38.3999V12.7999H129.8V38.3999H125.8Z"
        , Attributes.fill Colors.gray6CSS
        ]
        []
    ]


lotDebugCornerTopLeft : List (Svg msg)
lotDebugCornerTopLeft =
    [ Svg.rect
        [ Attributes.x "12"
        , Attributes.y "12"
        , Attributes.width "232"
        , Attributes.height "232"
        , Attributes.fill "#F9F9E9"
        ]
        []
    , path
        [ Attributes.d "M30 210V30H210"
        , Attributes.stroke "#3D3434"
        , Attributes.strokeWidth "10"
        , Attributes.strokeLinecap "round"
        , Attributes.strokeLinejoin "round"
        , Attributes.fill "none"
        ]
        []
    ]


lotDebugCornerTopRight : List (Svg msg)
lotDebugCornerTopRight =
    [ Svg.rect
        [ Attributes.x "244"
        , Attributes.y "12"
        , Attributes.width "232"
        , Attributes.height "232"
        , Attributes.transform "rotate(90 244 12)"
        , Attributes.fill "#F9F9E9"
        ]
        []
    , path
        [ Attributes.d "M46 30L226 30L226 210"
        , Attributes.stroke "#3D3434"
        , Attributes.strokeWidth "10"
        , Attributes.strokeLinecap "round"
        , Attributes.strokeLinejoin "round"
        , Attributes.fill "none"
        ]
        []
    ]


lotDebugCornerBottomRight : List (Svg msg)
lotDebugCornerBottomRight =
    [ Svg.rect
        [ Attributes.x "244"
        , Attributes.y "244"
        , Attributes.width "232"
        , Attributes.height "232"
        , Attributes.transform "rotate(-180 244 244)"
        , Attributes.fill "#F9F9E9"
        ]
        []
    , path
        [ Attributes.d "M226 46L226 226L46 226"
        , Attributes.stroke "#3D3434"
        , Attributes.strokeWidth "10"
        , Attributes.strokeLinecap "round"
        , Attributes.strokeLinejoin "round"
        , Attributes.fill "none"
        ]
        []
    ]


lotDebugCornerBottomLeft : List (Svg msg)
lotDebugCornerBottomLeft =
    [ Svg.rect
        [ Attributes.x "12"
        , Attributes.y "244"
        , Attributes.width "232"
        , Attributes.height "232"
        , Attributes.transform "rotate(-90 12 244)"
        , Attributes.fill "#F9F9E9"
        ]
        []
    , path
        [ Attributes.d "M210 226L30 226L30 46"
        , Attributes.stroke "#3D3434"
        , Attributes.strokeWidth "10"
        , Attributes.strokeLinecap "round"
        , Attributes.strokeLinejoin "round"
        , Attributes.fill "none"
        ]
        []
    ]


lotDebugCenter : List (Svg msg)
lotDebugCenter =
    [ Svg.rect
        [ Attributes.x "12"
        , Attributes.y "12"
        , Attributes.width "232"
        , Attributes.height "232"
        , Attributes.fill "#F9F9E9"
        ]
        []
    ]


lotDebugDrivewayRight : List (Svg msg)
lotDebugDrivewayRight =
    [ Svg.rect
        [ Attributes.x "244"
        , Attributes.y "244"
        , Attributes.width "232"
        , Attributes.height "232"
        , Attributes.transform "rotate(-180 244 244)"
        , Attributes.fill "#766565"
        ]
        []
    , path
        [ Attributes.d "M226 46L226 226L46 226"
        , Attributes.stroke "#3D3434"
        , Attributes.strokeWidth "10"
        , Attributes.strokeLinecap "round"
        , Attributes.strokeLinejoin "round"
        , Attributes.fill "none"
        ]
        []
    ]


lotDebugDrivewayLeft : List (Svg msg)
lotDebugDrivewayLeft =
    [ Svg.rect
        [ Attributes.x "12"
        , Attributes.y "244"
        , Attributes.width "232"
        , Attributes.height "232"
        , Attributes.transform "rotate(-90 12 244)"
        , Attributes.fill "#766565"
        ]
        []
    , path
        [ Attributes.d "M210 226L30 226L30 46"
        , Attributes.stroke "#3D3434"
        , Attributes.strokeWidth "10"
        , Attributes.strokeLinecap "round"
        , Attributes.strokeLinejoin "round"
        , Attributes.fill "none"
        ]
        []
    ]


lotDebugDrivewayUp : List (Svg msg)
lotDebugDrivewayUp =
    [ Svg.rect
        [ Attributes.x "12"
        , Attributes.y "244"
        , Attributes.width "232"
        , Attributes.height "232"
        , Attributes.transform "rotate(-90 12 244)"
        , Attributes.fill "#766565"
        ]
        []
    , path
        [ Attributes.d "M210 226L30 226L30 46"
        , Attributes.stroke "#3D3434"
        , Attributes.strokeWidth "10"
        , Attributes.strokeLinecap "round"
        , Attributes.strokeLinejoin "round"
        , Attributes.fill "none"
        ]
        []
    ]
