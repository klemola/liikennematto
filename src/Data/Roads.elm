module Data.Roads exposing (roadAsset)

import Data.Colors as Colors
import Svg exposing (Svg, path)
import Svg.Attributes as Attributes


roadAsset : Int -> List (Svg msg)
roadAsset tileIndex =
    case tileIndex of
        0 ->
            defaultRoad

        1 ->
            deadendDown

        2 ->
            deadendRight

        3 ->
            curveBottomRight

        4 ->
            deadendLeft

        5 ->
            curveBottomLeft

        6 ->
            regularHorizontal

        7 ->
            intersectionTUp

        8 ->
            deadendUp

        9 ->
            regularVertical

        10 ->
            curveTopRight

        11 ->
            intersectionTLeft

        12 ->
            curveTopLeft

        13 ->
            intersectionTRight

        14 ->
            intersectionTDown

        15 ->
            intersectionCrossroads

        _ ->
            defaultRoad


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
        [ Attributes.fill Colors.gray5CSS
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
        [ Attributes.fill Colors.gray5CSS
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
        [ Attributes.fill Colors.gray5CSS
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
        [ Attributes.fill Colors.gray5CSS
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
        [ Attributes.fill Colors.gray5CSS
        , Attributes.d "M130.601 12.31l-3.98.396a131.715 131.715 0 004.967 25.006l3.83-1.156a127.38 127.38 0 01-2.995-12.007 127.496 127.496 0 01-1.822-12.239zM144.843 59.387l-3.529 1.883a129.19 129.19 0 0014.162 21.19l3.09-2.54a125.257 125.257 0 01-13.723-20.533zM176.046 97.378l-2.537 3.093a130.79 130.79 0 0021.189 14.166l1.889-3.527a126.732 126.732 0 01-20.541-13.732zM219.4 120.589l-1.165 3.826a129.196 129.196 0 0024.993 4.987l.392-3.981a125.215 125.215 0 01-24.22-4.832z"
        ]
        []
    ]


regularHorizontal : List (Svg msg)
regularHorizontal =
    [ path
        [ Attributes.fill Colors.gray3CSS
        , Attributes.d "M0 223.5V32h256v191.5z"
        ]
        []
    , path
        [ Attributes.fill Colors.gray2CSS
        , Attributes.d "M0 188V68h256v120z"
        ]
        []
    , path
        [ Attributes.fill Colors.gray5CSS
        , Attributes.d "M13 130h25.6v-4H13v4zm51.2 0h25.6v-4H64.2v4zm51.2 0H141v-4h-25.6v4zm51.2 0h25.6v-4h-25.6v4zm51.2 0h25.6v-4h-25.6v4z"
        ]
        []
    , path
        [ Attributes.stroke Colors.gray1CSS
        , Attributes.strokeWidth "3"
        , Attributes.fill "none"
        , Attributes.d "M0 30.5h256M0 225.5h256"
        ]
        []
    ]


intersectionTUp : List (Svg msg)
intersectionTUp =
    [ path
        [ Attributes.fill Colors.gray3CSS
        , Attributes.d "M0 32c17.673 0 32-14.327 32-32h192c0 17.673 14.327 32 32 32v192H0V32z"
        ]
        []
    , path
        [ Attributes.fill Colors.gray2CSS
        , Attributes.d "M188 0H68v36c0 17.673-14.327 32-32 32H0v120h256V68h-36c-17.673 0-32-14.327-32-32V0z"
        ]
        []
    , path
        [ Attributes.fill Colors.gray5CSS
        , Attributes.d "M125 6v32h6V6h-6zm22 0v32h6V6h-6zm22 0v32h6V6h-6zm-66 0v32h6V6h-6zM81 6v32h6V6h-6zm137 125h32v-6h-32v6zm0-22h32v-6h-32v6zm0-22h32v-6h-32v6zm0 66h32v-6h-32v6zm0 22h32v-6h-32v6zM6 131h32v-6H6v6zm0-22h32v-6H6v6zm0-22h32v-6H6v6zm0 66h32v-6H6v6zm0 22h32v-6H6v6z"
        ]
        []
    , path
        [ Attributes.fill Colors.gray1CSS
        , Attributes.d "M224 0c0 17.673 14.327 32 32 32v-3c-16.016 0-29-12.984-29-29h-3zM0 32c17.673 0 32-14.327 32-32h-3c0 16.016-12.984 29-29 29v3zm0 195h256v-3H0v3z"
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
        [ Attributes.fill Colors.gray5CSS
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
    [ path
        [ Attributes.fill Colors.gray3CSS
        , Attributes.d "M32.5 0H224v256H32.5z"
        ]
        []
    , path
        [ Attributes.fill Colors.gray2CSS
        , Attributes.d "M68 0h120v256H68z"
        ]
        []
    , path
        [ Attributes.fill Colors.gray5CSS
        , Attributes.d "M126 13v25.6h4V13h-4zm0 51.2v25.6h4V64.2h-4zm0 51.2V141h4v-25.6h-4zm0 51.2v25.6h4v-25.6h-4zm0 51.2v25.6h4v-25.6h-4z"
        ]
        []
    , path
        [ Attributes.stroke Colors.gray1CSS
        , Attributes.strokeWidth "3"
        , Attributes.fill "none"
        , Attributes.d "M225.5 0v256M30.5 0v256"
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
        [ Attributes.fill Colors.gray5CSS
        , Attributes.d "M125.399 243.69l3.98-.396a131.749 131.749 0 00-4.967-25.005l-3.83 1.155a127.411 127.411 0 012.995 12.007 127.438 127.438 0 011.822 12.239zM111.157 196.613l3.529-1.883a129.17 129.17 0 00-14.162-21.19l-3.09 2.54a125.252 125.252 0 0113.723 20.533zM79.954 158.622l2.537-3.093a130.774 130.774 0 00-21.19-14.166l-1.888 3.527a126.755 126.755 0 0120.541 13.732zM36.6 135.411l1.165-3.826a129.196 129.196 0 00-24.993-4.987l-.392 3.981a125.208 125.208 0 0124.22 4.832z"
        ]
        []
    ]


intersectionTLeft : List (Svg msg)
intersectionTLeft =
    [ path
        [ Attributes.fill Colors.gray3CSS
        , Attributes.d "M32 256c0-17.673-14.327-32-32-32V32c17.673 0 32-14.327 32-32h192v256H32z"
        ]
        []
    , path
        [ Attributes.fill Colors.gray2CSS
        , Attributes.d "M0 68v120h36c17.673 0 32 14.327 32 32v36h120V0H68v36c0 17.673-14.327 32-32 32H0z"
        ]
        []
    , path
        [ Attributes.fill Colors.gray5CSS
        , Attributes.d "M6 131h32v-6H6v6zm0-22h32v-6H6v6zm0-22h32v-6H6v6zm0 66h32v-6H6v6zm0 22h32v-6H6v6zM131 38V6h-6v32h6zm-22 0V6h-6v32h6zm-22 0V6h-6v32h6zm66 0V6h-6v32h6zm22 0V6h-6v32h6zm-44 212v-32h-6v32h6zm-22 0v-32h-6v32h6zm-22 0v-32h-6v32h6zm66 0v-32h-6v32h6zm22 0v-32h-6v32h6z"
        ]
        []
    , path
        [ Attributes.fill Colors.gray1CSS
        , Attributes.d "M0 32c17.673 0 32-14.327 32-32h-3c0 16.016-12.984 29-29 29v3zm32 224c0-17.673-14.327-32-32-32v3c16.016 0 29 12.984 29 29h3zm195 0V0h-3v256h3z"
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
        [ Attributes.fill Colors.gray5CSS
        , Attributes.d "M130.601 243.69l-3.98-.396a131.749 131.749 0 014.967-25.005l3.83 1.155a127.411 127.411 0 00-2.995 12.007 127.438 127.438 0 00-1.822 12.239zM144.843 196.613l-3.529-1.883a129.17 129.17 0 0114.162-21.19l3.09 2.54a125.276 125.276 0 00-13.723 20.533zM176.046 158.622l-2.537-3.093a130.79 130.79 0 0121.189-14.166l1.889 3.527a126.72 126.72 0 00-20.541 13.732zM219.4 135.411l-1.165-3.826a129.196 129.196 0 0124.993-4.987l.392 3.981a125.215 125.215 0 00-24.22 4.832z"
        ]
        []
    ]


intersectionTRight : List (Svg msg)
intersectionTRight =
    [ path
        [ Attributes.fill Colors.gray3CSS
        , Attributes.d "M224 0c0 17.673 14.327 32 32 32v192c-17.673 0-32 14.327-32 32H32V0h192z"
        ]
        []
    , path
        [ Attributes.fill Colors.gray2CSS
        , Attributes.d "M256 188V68h-36c-17.673 0-32-14.327-32-32V0H68v256h120v-36c0-17.673 14.327-32 32-32h36z"
        ]
        []
    , path
        [ Attributes.fill Colors.gray5CSS
        , Attributes.d "M250 125h-32v6h32v-6zm0 22h-32v6h32v-6zm0 22h-32v6h32v-6zm0-66h-32v6h32v-6zm0-22h-32v6h32v-6zM125 218v32h6v-32h-6zm22 0v32h6v-32h-6zm22 0v32h6v-32h-6zm-66 0v32h6v-32h-6zm-22 0v32h6v-32h-6zM125 6v32h6V6h-6zm22 0v32h6V6h-6zm22 0v32h6V6h-6zm-66 0v32h6V6h-6zM81 6v32h6V6h-6z"
        ]
        []
    , path
        [ Attributes.fill Colors.gray1CSS
        , Attributes.d "M256 224c-17.673 0-32 14.327-32 32h3c0-16.016 12.984-29 29-29v-3zM224 0c0 17.673 14.327 32 32 32v-3c-16.016 0-29-12.984-29-29h-3zM29 0v256h3V0h-3z"
        ]
        []
    ]


intersectionTDown : List (Svg msg)
intersectionTDown =
    [ path
        [ Attributes.fill Colors.gray3CSS
        , Attributes.d "M256 224c-17.673 0-32 14.327-32 32H32c0-17.673-14.327-32-32-32V32h256v192z"
        ]
        []
    , path
        [ Attributes.fill Colors.gray2CSS
        , Attributes.d "M68 256h120v-36c0-17.673 14.327-32 32-32h36V68H0v120h36c17.673 0 32 14.327 32 32v36z"
        ]
        []
    , path
        [ Attributes.fill Colors.gray5CSS
        , Attributes.d "M131 250v-32h-6v32h6zm-22 0v-32h-6v32h6zm-22 0v-32h-6v32h6zm66 0v-32h-6v32h6zm22 0v-32h-6v32h6zM38 125H6v6h32v-6zm0 22H6v6h32v-6zm0 22H6v6h32v-6zm0-66H6v6h32v-6zm0-22H6v6h32v-6zm212 44h-32v6h32v-6zm0 22h-32v6h32v-6zm0 22h-32v6h32v-6zm0-66h-32v6h32v-6zm0-22h-32v6h32v-6z"
        ]
        []
    , path
        [ Attributes.fill Colors.gray1CSS
        , Attributes.d "M32 256c0-17.673-14.327-32-32-32v3c16.016 0 29 12.984 29 29h3zm224-32c-17.673 0-32 14.327-32 32h3c0-16.016 12.984-29 29-29v-3zm0-195H0v3h256v-3z"
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
        [ Attributes.fill Colors.gray5CSS
        , Attributes.d "M125 6v32h6V6h-6zm22 0v32h6V6h-6zm22 0v32h6V6h-6zm-66 0v32h6V6h-6zM81 6v32h6V6h-6zm44 212v32h6v-32h-6zm22 0v32h6v-32h-6zm22 0v32h6v-32h-6zm-66 0v32h6v-32h-6zm-22 0v32h6v-32h-6zm137-87h32v-6h-32v6zm0-22h32v-6h-32v6zm0-22h32v-6h-32v6zm0 66h32v-6h-32v6zm0 22h32v-6h-32v6zM6 131h32v-6H6v6zm0-22h32v-6H6v6zm0-22h32v-6H6v6zm0 66h32v-6H6v6zm0 22h32v-6H6v6z"
        ]
        []
    , path
        [ Attributes.fill Colors.gray1CSS
        , Attributes.d "M255.496 30.492l-.025 1.5.025-1.5zm-.025 1.5l.504.008.05-3-.504-.008-.05 3zM224 0c0 17.466 14.007 31.705 31.471 31.991l.05-3C239.694 28.734 227 15.83 227 0h-3zM32 256c0-17.673-14.327-32-32-32v3c16.016 0 29 12.984 29 29h3zm224-32c-17.673 0-32 14.327-32 32h3c0-16.016 12.984-29 29-29v-3zM0 32c17.673 0 32-14.327 32-32h-3c0 16.016-12.984 29-29 29v3z"
        ]
        []
    ]
