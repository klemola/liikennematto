module Data.RoadAssets exposing (roadAsset)

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


defaultRoad =
    [ Svg.circle
        [ Attributes.cx "128"
        , Attributes.cy "128"
        , Attributes.r "98"
        , Attributes.fill "#BCA9A9"
        , Attributes.stroke "#302525"
        , Attributes.strokeWidth "4"
        , Attributes.class "sidewalk"
        ]
        []
    , Svg.circle
        [ Attributes.cx "128"
        , Attributes.cy "128"
        , Attributes.r "60"
        , Attributes.fill "#785E5E"
        , Attributes.class "road"
        ]
        []
    ]


deadendDown =
    [ path
        [ Attributes.fill "#BCA9A9"
        , Attributes.d "M32 0v128a95.998 95.998 0 0096 96 95.996 95.996 0 0067.882-28.118A95.996 95.996 0 00224 128V0h-36v128a60.008 60.008 0 01-17.574 42.426 59.993 59.993 0 01-65.387 13.007A60.006 60.006 0 0168 128V0H32z"
        , Attributes.class "Union"
        ]
        []
    , path
        [ Attributes.fill "#785E5E"
        , Attributes.d "M188 0H68v128c0 33.137 26.863 60 60 60s60-26.863 60-60V0z"
        , Attributes.class "road"
        ]
        []
    , Svg.g
        [ Attributes.class "markings"
        ]
        [ path
            [ Attributes.stroke "#F0F0DD"
            , Attributes.strokeWidth "4"
            , Attributes.d "M128 13v25.6"
            , Attributes.class "markings_2"
            ]
            []
        , path
            [ Attributes.stroke "#F0F0DD"
            , Attributes.strokeWidth "4"
            , Attributes.d "M128 64.2v25.6"
            , Attributes.class "markings_3"
            ]
            []
        ]
    , path
        [ Attributes.stroke "#302525"
        , Attributes.strokeWidth "3"
        , Attributes.fill "none"
        , Attributes.d "M30.5 0v126.867c0 56.513 45.04 98.633 97.5 98.633 52.459 0 97.5-42.119 97.5-98.633V0"
        , Attributes.class "outline"
        ]
        []
    ]


deadendRight =
    [ path
        [ Attributes.fill "#BCA9A9"
        , Attributes.d "M0 224h128a95.996 95.996 0 0067.882-28.118 95.996 95.996 0 000-135.764A95.998 95.998 0 00128 32H0v36h128a60.003 60.003 0 0160 60 60.008 60.008 0 01-17.574 42.426A59.993 59.993 0 01128 188H0v36z"
        , Attributes.class "Union"
        ]
        []
    , path
        [ Attributes.fill "#785E5E"
        , Attributes.d "M0 68v120h128c33.137 0 60-26.863 60-60s-26.863-60-60-60H0z"
        , Attributes.class "road"
        ]
        []
    , Svg.g
        [ Attributes.class "markings"
        ]
        [ path
            [ Attributes.stroke "#F0F0DD"
            , Attributes.strokeWidth "4"
            , Attributes.d "M13 128h25.6"
            , Attributes.class "markings_2"
            ]
            []
        , path
            [ Attributes.stroke "#F0F0DD"
            , Attributes.strokeWidth "4"
            , Attributes.d "M64.2 128h25.6"
            , Attributes.class "markings_3"
            ]
            []
        ]
    , path
        [ Attributes.stroke "#302525"
        , Attributes.strokeWidth "3"
        , Attributes.fill "none"
        , Attributes.d "M0 225.5h126.867c56.513 0 98.633-45.041 98.633-97.5 0-52.46-42.119-97.5-98.633-97.5H0"
        , Attributes.class "outline"
        ]
        []
    ]


curveBottomRight =
    [ path
        [ Attributes.fill "#785E5E"
        , Attributes.d "M0 188c103.83 0 188-84.17 188-188H68c0 37.555-30.445 68-68 68v120z"
        , Attributes.class "sidewalk_outer"
        ]
        []
    , path
        [ Attributes.fill "#BCA9A9"
        , Attributes.d "M0 224c123.712 0 224-100.288 224-224h-36c0 103.83-84.17 188-188 188v36z"
        , Attributes.class "road"
        ]
        []
    , path
        [ Attributes.fill "#BCA9A9"
        , Attributes.d "M0 68c37.555 0 68-30.445 68-68H32c0 17.673-14.327 32-32 32v36z"
        , Attributes.class "sidewalk_inner"
        ]
        []
    , path
        [ Attributes.stroke "#302525"
        , Attributes.strokeWidth "3"
        , Attributes.fill "none"
        , Attributes.d "M0 30.5v0c16.845 0 30.5-13.655 30.5-30.5v0"
        , Attributes.class "outline"
        ]
        []
    , path
        [ Attributes.stroke "#302525"
        , Attributes.strokeWidth "3"
        , Attributes.fill "none"
        , Attributes.d "M0 225.5v0c124.54 0 225.5-100.96 225.5-225.5v0"
        , Attributes.class "outline_2"
        ]
        []
    , path
        [ Attributes.fill "#F0F0DD"
        , Attributes.fillRule "evenodd"
        , Attributes.d "M12.31 125.399l.396 3.98a131.715 131.715 0 0025.006-4.967l-1.156-3.83a127.688 127.688 0 01-12.007 2.995 127.496 127.496 0 01-12.239 1.822zm47.077-14.242l1.883 3.529a129.194 129.194 0 0021.19-14.162l-2.54-3.09a125.224 125.224 0 01-20.533 13.723zm37.991-31.203l3.093 2.537a130.708 130.708 0 0014.165-21.19l-3.526-1.888a126.767 126.767 0 01-13.732 20.541zM120.59 36.6l3.826 1.165a129.196 129.196 0 004.987-24.993l-3.981-.392a125.208 125.208 0 01-4.832 24.22z"
        , Attributes.class "markings"
        , Attributes.clipRule "evenodd"
        ]
        []
    ]


deadendLeft =
    [ path
        [ Attributes.fill "#BCA9A9"
        , Attributes.d "M256 32H128a96 96 0 100 192h128v-36H128a60.008 60.008 0 01-42.426-17.574A59.989 59.989 0 0168 128a60.003 60.003 0 0160-60h128V32z"
        , Attributes.class "Union"
        ]
        []
    , path
        [ Attributes.fill "#785E5E"
        , Attributes.d "M256 188V68H128c-33.137 0-60 26.863-60 60s26.863 60 60 60h128z"
        , Attributes.class "road"
        ]
        []
    , Svg.g
        [ Attributes.class "markings"
        ]
        [ path
            [ Attributes.stroke "#F0F0DD"
            , Attributes.strokeWidth "4"
            , Attributes.d "M243 128h-25.6"
            , Attributes.class "markings_2"
            ]
            []
        , path
            [ Attributes.stroke "#F0F0DD"
            , Attributes.strokeWidth "4"
            , Attributes.d "M191.8 128h-25.6"
            , Attributes.class "markings_3"
            ]
            []
        ]
    , path
        [ Attributes.stroke "#302525"
        , Attributes.strokeWidth "3"
        , Attributes.fill "none"
        , Attributes.d "M256 30.5H129.133C72.62 30.5 30.5 75.54 30.5 128c0 52.459 42.12 97.5 98.633 97.5H256"
        , Attributes.class "outline"
        ]
        []
    ]


curveBottomLeft =
    [ path
        [ Attributes.fill "#785E5E"
        , Attributes.d "M68 0c0 103.83 84.17 188 188 188V68c-37.555 0-68-30.445-68-68H68z"
        , Attributes.class "sidewalk_outer"
        ]
        []
    , path
        [ Attributes.fill "#BCA9A9"
        , Attributes.d "M32 0c0 123.712 100.288 224 224 224v-36C152.17 188 68 103.83 68 0H32z"
        , Attributes.class "road"
        ]
        []
    , path
        [ Attributes.fill "#BCA9A9"
        , Attributes.d "M188 0c0 37.555 30.445 68 68 68V32c-17.673 0-32-14.327-32-32h-36z"
        , Attributes.class "sidewalk_inner"
        ]
        []
    , path
        [ Attributes.stroke "#302525"
        , Attributes.strokeWidth "3"
        , Attributes.fill "none"
        , Attributes.d "M225.5 0v0c0 16.845 13.655 30.5 30.5 30.5v0"
        , Attributes.class "outline"
        ]
        []
    , path
        [ Attributes.stroke "#302525"
        , Attributes.strokeWidth "3"
        , Attributes.fill "none"
        , Attributes.d "M30.5 0v0c0 124.54 100.96 225.5 225.5 225.5v0"
        , Attributes.class "outline_2"
        ]
        []
    , path
        [ Attributes.fill "#F0F0DD"
        , Attributes.fillRule "evenodd"
        , Attributes.d "M130.601 12.31l-3.98.396a131.636 131.636 0 004.968 25.005l3.829-1.155a127.688 127.688 0 01-2.995-12.007 127.496 127.496 0 01-1.822-12.239zm14.242 47.077l-3.529 1.883a129.253 129.253 0 0014.162 21.19l3.091-2.54a125.198 125.198 0 01-13.724-20.533zm31.203 37.991l-2.537 3.093a130.718 130.718 0 0021.189 14.165l1.889-3.526a126.732 126.732 0 01-20.541-13.732zM219.4 120.59l-1.165 3.826a129.196 129.196 0 0024.993 4.987l.392-3.981a125.215 125.215 0 01-24.22-4.832z"
        , Attributes.class "markings"
        , Attributes.clipRule "evenodd"
        ]
        []
    ]


regularHorizontal =
    [ path
        [ Attributes.fill "#785E5E"
        , Attributes.d "M0 188V68h256v120z"
        , Attributes.class "road"
        ]
        []
    , Svg.g
        [ Attributes.class "markings"
        ]
        [ path
            [ Attributes.stroke "#F0F0DD"
            , Attributes.strokeWidth "4"
            , Attributes.d "M13 128h25.6"
            , Attributes.class "markings_2"
            ]
            []
        , path
            [ Attributes.stroke "#F0F0DD"
            , Attributes.strokeWidth "4"
            , Attributes.d "M64.2 128h25.6"
            , Attributes.class "markings_3"
            ]
            []
        , path
            [ Attributes.stroke "#F0F0DD"
            , Attributes.strokeWidth "4"
            , Attributes.d "M115.4 128H141"
            , Attributes.class "markings_4"
            ]
            []
        , path
            [ Attributes.stroke "#F0F0DD"
            , Attributes.strokeWidth "4"
            , Attributes.d "M166.6 128h25.6"
            , Attributes.class "markings_5"
            ]
            []
        , path
            [ Attributes.stroke "#F0F0DD"
            , Attributes.strokeWidth "4"
            , Attributes.d "M217.8 128h25.6"
            , Attributes.class "markings_6"
            ]
            []
        ]
    , path
        [ Attributes.stroke "#BCA9A9"
        , Attributes.strokeWidth "36"
        , Attributes.d "M0 50h256M0 206h256"
        , Attributes.class "sidewalk"
        ]
        []
    , path
        [ Attributes.stroke "#302525"
        , Attributes.strokeWidth "3"
        , Attributes.fill "none"
        , Attributes.d "M0 30.5h256M0 225.5h256"
        , Attributes.class "outline"
        ]
        []
    ]


intersectionTUp =
    [ path
        [ Attributes.fill "#785E5E"
        , Attributes.d "M188 0H68v36c0 17.673-14.327 32-32 32H0v120h256V68h-36c-17.673 0-32-14.327-32-32V0z"
        , Attributes.class "road"
        ]
        []
    , Svg.g
        [ Attributes.class "markings"
        ]
        [ path
            [ Attributes.stroke "#F0F0DD"
            , Attributes.strokeWidth "6"
            , Attributes.d "M128 6v32"
            , Attributes.class "markings_2"
            ]
            []
        , path
            [ Attributes.stroke "#F0F0DD"
            , Attributes.strokeWidth "6"
            , Attributes.d "M150 6v32"
            , Attributes.class "markings_3"
            ]
            []
        , path
            [ Attributes.stroke "#F0F0DD"
            , Attributes.strokeWidth "6"
            , Attributes.d "M172 6v32"
            , Attributes.class "markings_4"
            ]
            []
        , path
            [ Attributes.stroke "#F0F0DD"
            , Attributes.strokeWidth "6"
            , Attributes.d "M106 6v32"
            , Attributes.class "markings_5"
            ]
            []
        , path
            [ Attributes.stroke "#F0F0DD"
            , Attributes.strokeWidth "6"
            , Attributes.d "M84 6v32"
            , Attributes.class "markings_6"
            ]
            []
        , path
            [ Attributes.stroke "#F0F0DD"
            , Attributes.strokeWidth "6"
            , Attributes.d "M218 128h32"
            , Attributes.class "markings_7"
            ]
            []
        , path
            [ Attributes.stroke "#F0F0DD"
            , Attributes.strokeWidth "6"
            , Attributes.d "M218 106h32"
            , Attributes.class "markings_8"
            ]
            []
        , path
            [ Attributes.stroke "#F0F0DD"
            , Attributes.strokeWidth "6"
            , Attributes.d "M218 84h32"
            , Attributes.class "markings_9"
            ]
            []
        , path
            [ Attributes.stroke "#F0F0DD"
            , Attributes.strokeWidth "6"
            , Attributes.d "M218 150h32"
            , Attributes.class "markings_10"
            ]
            []
        , path
            [ Attributes.stroke "#F0F0DD"
            , Attributes.strokeWidth "6"
            , Attributes.d "M218 172h32"
            , Attributes.class "markings_11"
            ]
            []
        , path
            [ Attributes.stroke "#F0F0DD"
            , Attributes.strokeWidth "6"
            , Attributes.d "M6 128h32"
            , Attributes.class "markings_12"
            ]
            []
        , path
            [ Attributes.stroke "#F0F0DD"
            , Attributes.strokeWidth "6"
            , Attributes.d "M6 106h32"
            , Attributes.class "markings_13"
            ]
            []
        , path
            [ Attributes.stroke "#F0F0DD"
            , Attributes.strokeWidth "6"
            , Attributes.d "M6 84h32"
            , Attributes.class "markings_14"
            ]
            []
        , path
            [ Attributes.stroke "#F0F0DD"
            , Attributes.strokeWidth "6"
            , Attributes.d "M6 150h32"
            , Attributes.class "markings_15"
            ]
            []
        , path
            [ Attributes.stroke "#F0F0DD"
            , Attributes.strokeWidth "6"
            , Attributes.d "M6 172h32"
            , Attributes.class "markings_16"
            ]
            []
        ]
    , path
        [ Attributes.stroke "#302525"
        , Attributes.strokeWidth "3"
        , Attributes.fill "none"
        , Attributes.d "M225.5 0v0c0 16.845 13.655 30.5 30.5 30.5v0"
        , Attributes.class "outline"
        ]
        []
    , path
        [ Attributes.stroke "#302525"
        , Attributes.strokeWidth "3"
        , Attributes.fill "none"
        , Attributes.d "M0 30.5v0c16.845 0 30.5-13.655 30.5-30.5v0"
        , Attributes.class "outline_2"
        ]
        []
    , path
        [ Attributes.fill "#BCA9A9"
        , Attributes.d "M188 0h36c0 17.673 14.327 32 32 32v36h-36c-17.673 0-32-14.327-32-32V0z"
        , Attributes.class "sidewalk"
        ]
        []
    , path
        [ Attributes.fill "#BCA9A9"
        , Attributes.d "M256 224H0v-36h256v36z"
        , Attributes.class "sidewalk_2"
        ]
        []
    , path
        [ Attributes.fill "#BCA9A9"
        , Attributes.d "M0 68V32c17.673 0 32-14.327 32-32h36v36c0 17.673-14.327 32-32 32H0z"
        , Attributes.class "sidewalk_3"
        ]
        []
    , path
        [ Attributes.stroke "#302525"
        , Attributes.strokeWidth "3"
        , Attributes.fill "none"
        , Attributes.d "M0 225.5h256"
        , Attributes.class "outline_3"
        ]
        []
    ]


deadendUp =
    [ path
        [ Attributes.fill "#BCA9A9"
        , Attributes.d "M224 256V128a95.998 95.998 0 00-96-96 96 96 0 00-96 96v128h36V128a60.003 60.003 0 0160-60 60.003 60.003 0 0160 60v128h36z"
        , Attributes.class "Union"
        ]
        []
    , path
        [ Attributes.fill "#785E5E"
        , Attributes.d "M68 256h120V128c0-33.137-26.863-60-60-60s-60 26.863-60 60v128z"
        , Attributes.class "road"
        ]
        []
    , Svg.g
        [ Attributes.class "markings"
        ]
        [ path
            [ Attributes.stroke "#F0F0DD"
            , Attributes.strokeWidth "4"
            , Attributes.d "M128 243v-25.6"
            , Attributes.class "markings_2"
            ]
            []
        , path
            [ Attributes.stroke "#F0F0DD"
            , Attributes.strokeWidth "4"
            , Attributes.d "M128 191.8v-25.6"
            , Attributes.class "markings_3"
            ]
            []
        ]
    , path
        [ Attributes.stroke "#302525"
        , Attributes.strokeWidth "3"
        , Attributes.fill "none"
        , Attributes.d "M225.5 256V129.133C225.5 72.62 180.459 30.5 128 30.5c-52.46 0-97.5 42.12-97.5 98.633V256"
        , Attributes.class "outline"
        ]
        []
    ]


regularVertical =
    [ path
        [ Attributes.fill "#785E5E"
        , Attributes.d "M68 0h120v256H68z"
        , Attributes.class "road"
        ]
        []
    , Svg.g
        [ Attributes.class "markings"
        ]
        [ path
            [ Attributes.stroke "#F0F0DD"
            , Attributes.strokeWidth "4"
            , Attributes.d "M128 13v25.6"
            , Attributes.class "markings_2"
            ]
            []
        , path
            [ Attributes.stroke "#F0F0DD"
            , Attributes.strokeWidth "4"
            , Attributes.d "M128 64.2v25.6"
            , Attributes.class "markings_3"
            ]
            []
        , path
            [ Attributes.stroke "#F0F0DD"
            , Attributes.strokeWidth "4"
            , Attributes.d "M128 115.4V141"
            , Attributes.class "markings_4"
            ]
            []
        , path
            [ Attributes.stroke "#F0F0DD"
            , Attributes.strokeWidth "4"
            , Attributes.d "M128 166.6v25.6"
            , Attributes.class "markings_5"
            ]
            []
        , path
            [ Attributes.stroke "#F0F0DD"
            , Attributes.strokeWidth "4"
            , Attributes.d "M128 217.8v25.6"
            , Attributes.class "markings_6"
            ]
            []
        ]
    , path
        [ Attributes.stroke "#BCA9A9"
        , Attributes.strokeWidth "36"
        , Attributes.d "M206 0v256M50 0v256"
        , Attributes.class "sidewalk"
        ]
        []
    , path
        [ Attributes.stroke "#302525"
        , Attributes.strokeWidth "3"
        , Attributes.fill "none"
        , Attributes.d "M225.5 0v256M30.5 0v256"
        , Attributes.class "outline"
        ]
        []
    ]


curveTopRight =
    [ path
        [ Attributes.fill "#785E5E"
        , Attributes.d "M188 256C188 152.17 103.83 68 0 68v120c37.555 0 68 30.445 68 68h120z"
        , Attributes.class "sidewalk_outer"
        ]
        []
    , path
        [ Attributes.fill "#BCA9A9"
        , Attributes.d "M224 256C224 132.288 123.712 32 0 32v36c103.83 0 188 84.17 188 188h36z"
        , Attributes.class "road"
        ]
        []
    , path
        [ Attributes.fill "#BCA9A9"
        , Attributes.d "M68 256c0-37.555-30.445-68-68-68v36c17.673 0 32 14.327 32 32h36z"
        , Attributes.class "sidewalk_inner"
        ]
        []
    , path
        [ Attributes.stroke "#302525"
        , Attributes.strokeWidth "3"
        , Attributes.fill "none"
        , Attributes.d "M30.5 256v0c0-16.845-13.655-30.5-30.5-30.5v0"
        , Attributes.class "outline"
        ]
        []
    , path
        [ Attributes.stroke "#302525"
        , Attributes.strokeWidth "3"
        , Attributes.fill "none"
        , Attributes.d "M225.5 256v0C225.5 131.46 124.54 30.5 0 30.5v0"
        , Attributes.class "outline_2"
        ]
        []
    , path
        [ Attributes.fill "#F0F0DD"
        , Attributes.fillRule "evenodd"
        , Attributes.d "M125.399 243.69l3.98-.396a131.655 131.655 0 00-4.968-25.005l-3.829 1.155a127.72 127.72 0 012.995 12.007 127.438 127.438 0 011.822 12.239zm-14.242-47.077l3.529-1.883a129.234 129.234 0 00-14.162-21.19l-3.09 2.54a125.24 125.24 0 0113.723 20.533zm-31.203-37.991l2.537-3.093a130.703 130.703 0 00-21.19-14.165l-1.888 3.526a126.755 126.755 0 0120.541 13.732zM36.6 135.411l1.165-3.826a129.193 129.193 0 00-24.994-4.987l-.391 3.981a125.21 125.21 0 0124.22 4.832z"
        , Attributes.class "markings"
        , Attributes.clipRule "evenodd"
        ]
        []
    ]


intersectionTLeft =
    [ path
        [ Attributes.fill "#785E5E"
        , Attributes.d "M0 68v120h36c17.673 0 32 14.327 32 32v36h120V0H68v36c0 17.673-14.327 32-32 32H0z"
        , Attributes.class "road"
        ]
        []
    , Svg.g
        [ Attributes.class "markings"
        ]
        [ path
            [ Attributes.stroke "#F0F0DD"
            , Attributes.strokeWidth "6"
            , Attributes.d "M6 128h32"
            , Attributes.class "markings_2"
            ]
            []
        , path
            [ Attributes.stroke "#F0F0DD"
            , Attributes.strokeWidth "6"
            , Attributes.d "M6 106h32"
            , Attributes.class "markings_3"
            ]
            []
        , path
            [ Attributes.stroke "#F0F0DD"
            , Attributes.strokeWidth "6"
            , Attributes.d "M6 84h32"
            , Attributes.class "markings_4"
            ]
            []
        , path
            [ Attributes.stroke "#F0F0DD"
            , Attributes.strokeWidth "6"
            , Attributes.d "M6 150h32"
            , Attributes.class "markings_5"
            ]
            []
        , path
            [ Attributes.stroke "#F0F0DD"
            , Attributes.strokeWidth "6"
            , Attributes.d "M6 172h32"
            , Attributes.class "markings_6"
            ]
            []
        , path
            [ Attributes.stroke "#F0F0DD"
            , Attributes.strokeWidth "6"
            , Attributes.d "M128 38V6"
            , Attributes.class "markings_7"
            ]
            []
        , path
            [ Attributes.stroke "#F0F0DD"
            , Attributes.strokeWidth "6"
            , Attributes.d "M106 38V6"
            , Attributes.class "markings_8"
            ]
            []
        , path
            [ Attributes.stroke "#F0F0DD"
            , Attributes.strokeWidth "6"
            , Attributes.d "M84 38V6"
            , Attributes.class "markings_9"
            ]
            []
        , path
            [ Attributes.stroke "#F0F0DD"
            , Attributes.strokeWidth "6"
            , Attributes.d "M150 38V6"
            , Attributes.class "markings_10"
            ]
            []
        , path
            [ Attributes.stroke "#F0F0DD"
            , Attributes.strokeWidth "6"
            , Attributes.d "M172 38V6"
            , Attributes.class "markings_11"
            ]
            []
        , path
            [ Attributes.stroke "#F0F0DD"
            , Attributes.strokeWidth "6"
            , Attributes.d "M128 250v-32"
            , Attributes.class "markings_12"
            ]
            []
        , path
            [ Attributes.stroke "#F0F0DD"
            , Attributes.strokeWidth "6"
            , Attributes.d "M106 250v-32"
            , Attributes.class "markings_13"
            ]
            []
        , path
            [ Attributes.stroke "#F0F0DD"
            , Attributes.strokeWidth "6"
            , Attributes.d "M84 250v-32"
            , Attributes.class "markings_14"
            ]
            []
        , path
            [ Attributes.stroke "#F0F0DD"
            , Attributes.strokeWidth "6"
            , Attributes.d "M150 250v-32"
            , Attributes.class "markings_15"
            ]
            []
        , path
            [ Attributes.stroke "#F0F0DD"
            , Attributes.strokeWidth "6"
            , Attributes.d "M172 250v-32"
            , Attributes.class "markings_16"
            ]
            []
        ]
    , path
        [ Attributes.stroke "#302525"
        , Attributes.strokeWidth "3"
        , Attributes.fill "none"
        , Attributes.d "M0 30.5v0c16.845 0 30.5-13.655 30.5-30.5v0"
        , Attributes.class "outline"
        ]
        []
    , path
        [ Attributes.stroke "#302525"
        , Attributes.strokeWidth "3"
        , Attributes.fill "none"
        , Attributes.d "M30.5 256v0c0-16.845-13.655-30.5-30.5-30.5v0"
        , Attributes.class "outline_2"
        ]
        []
    , path
        [ Attributes.fill "#BCA9A9"
        , Attributes.d "M0 68V32c17.673 0 32-14.327 32-32h36v36c0 17.673-14.327 32-32 32H0z"
        , Attributes.class "sidewalk"
        ]
        []
    , path
        [ Attributes.fill "#BCA9A9"
        , Attributes.d "M224 0v256h-36V0h36z"
        , Attributes.class "sidewalk_2"
        ]
        []
    , path
        [ Attributes.fill "#BCA9A9"
        , Attributes.d "M68 256H32c0-17.673-14.327-32-32-32v-36h36c17.673 0 32 14.327 32 32v36z"
        , Attributes.class "sidewalk_3"
        ]
        []
    , path
        [ Attributes.stroke "#302525"
        , Attributes.strokeWidth "3"
        , Attributes.fill "none"
        , Attributes.d "M225.5 256V0"
        , Attributes.class "outline_3"
        ]
        []
    ]


curveTopLeft =
    [ path
        [ Attributes.fill "#785E5E"
        , Attributes.d "M68 256c0-103.83 84.17-188 188-188v120c-37.555 0-68 30.445-68 68H68z"
        , Attributes.class "sidewalk_outer"
        ]
        []
    , path
        [ Attributes.fill "#BCA9A9"
        , Attributes.d "M32 256C32 132.288 132.288 32 256 32v36C152.17 68 68 152.17 68 256H32z"
        , Attributes.class "road"
        ]
        []
    , path
        [ Attributes.fill "#BCA9A9"
        , Attributes.d "M188 256c0-37.555 30.445-68 68-68v36c-17.673 0-32 14.327-32 32h-36z"
        , Attributes.class "sidewalk_inner"
        ]
        []
    , path
        [ Attributes.stroke "#302525"
        , Attributes.strokeWidth "3"
        , Attributes.fill "none"
        , Attributes.d "M225.5 256v0c0-16.845 13.655-30.5 30.5-30.5v0"
        , Attributes.class "outline"
        ]
        []
    , path
        [ Attributes.stroke "#302525"
        , Attributes.strokeWidth "3"
        , Attributes.fill "none"
        , Attributes.d "M30.5 256v0C30.5 131.46 131.46 30.5 256 30.5v0"
        , Attributes.class "outline_2"
        ]
        []
    , path
        [ Attributes.fill "#F0F0DD"
        , Attributes.fillRule "evenodd"
        , Attributes.d "M130.601 243.69l-3.98-.396a131.655 131.655 0 014.968-25.005l3.829 1.155a127.72 127.72 0 00-2.995 12.007 127.438 127.438 0 00-1.822 12.239zm14.242-47.077l-3.529-1.883a129.234 129.234 0 0114.162-21.19l3.091 2.54a125.217 125.217 0 00-13.724 20.533zm31.203-37.991l-2.537-3.093a130.718 130.718 0 0121.189-14.165l1.889 3.526a126.72 126.72 0 00-20.541 13.732zm43.354-23.211l-1.165-3.826a129.196 129.196 0 0124.993-4.987l.392 3.981a125.215 125.215 0 00-24.22 4.832z"
        , Attributes.class "markings"
        , Attributes.clipRule "evenodd"
        ]
        []
    ]


intersectionTRight =
    [ path
        [ Attributes.fill "#785E5E"
        , Attributes.d "M256 188V68h-36c-17.673 0-32-14.327-32-32V0H68v256h120v-36c0-17.673 14.327-32 32-32h36z"
        , Attributes.class "road"
        ]
        []
    , Svg.g
        [ Attributes.class "markings"
        ]
        [ path
            [ Attributes.stroke "#F0F0DD"
            , Attributes.strokeWidth "6"
            , Attributes.d "M250 128h-32"
            , Attributes.class "markings_2"
            ]
            []
        , path
            [ Attributes.stroke "#F0F0DD"
            , Attributes.strokeWidth "6"
            , Attributes.d "M250 150h-32"
            , Attributes.class "markings_3"
            ]
            []
        , path
            [ Attributes.stroke "#F0F0DD"
            , Attributes.strokeWidth "6"
            , Attributes.d "M250 172h-32"
            , Attributes.class "markings_4"
            ]
            []
        , path
            [ Attributes.stroke "#F0F0DD"
            , Attributes.strokeWidth "6"
            , Attributes.d "M250 106h-32"
            , Attributes.class "markings_5"
            ]
            []
        , path
            [ Attributes.stroke "#F0F0DD"
            , Attributes.strokeWidth "6"
            , Attributes.d "M250 84h-32"
            , Attributes.class "markings_6"
            ]
            []
        , path
            [ Attributes.stroke "#F0F0DD"
            , Attributes.strokeWidth "6"
            , Attributes.d "M128 218v32"
            , Attributes.class "markings_7"
            ]
            []
        , path
            [ Attributes.stroke "#F0F0DD"
            , Attributes.strokeWidth "6"
            , Attributes.d "M150 218v32"
            , Attributes.class "markings_8"
            ]
            []
        , path
            [ Attributes.stroke "#F0F0DD"
            , Attributes.strokeWidth "6"
            , Attributes.d "M172 218v32"
            , Attributes.class "markings_9"
            ]
            []
        , path
            [ Attributes.stroke "#F0F0DD"
            , Attributes.strokeWidth "6"
            , Attributes.d "M106 218v32"
            , Attributes.class "markings_10"
            ]
            []
        , path
            [ Attributes.stroke "#F0F0DD"
            , Attributes.strokeWidth "6"
            , Attributes.d "M84 218v32"
            , Attributes.class "markings_11"
            ]
            []
        , path
            [ Attributes.stroke "#F0F0DD"
            , Attributes.strokeWidth "6"
            , Attributes.d "M128 6v32"
            , Attributes.class "markings_12"
            ]
            []
        , path
            [ Attributes.stroke "#F0F0DD"
            , Attributes.strokeWidth "6"
            , Attributes.d "M150 6v32"
            , Attributes.class "markings_13"
            ]
            []
        , path
            [ Attributes.stroke "#F0F0DD"
            , Attributes.strokeWidth "6"
            , Attributes.d "M172 6v32"
            , Attributes.class "markings_14"
            ]
            []
        , path
            [ Attributes.stroke "#F0F0DD"
            , Attributes.strokeWidth "6"
            , Attributes.d "M106 6v32"
            , Attributes.class "markings_15"
            ]
            []
        , path
            [ Attributes.stroke "#F0F0DD"
            , Attributes.strokeWidth "6"
            , Attributes.d "M84 6v32"
            , Attributes.class "markings_16"
            ]
            []
        ]
    , path
        [ Attributes.stroke "#302525"
        , Attributes.strokeWidth "3"
        , Attributes.fill "none"
        , Attributes.d "M256 225.5v0c-16.845 0-30.5 13.655-30.5 30.5v0"
        , Attributes.class "outline"
        ]
        []
    , path
        [ Attributes.stroke "#302525"
        , Attributes.strokeWidth "3"
        , Attributes.fill "none"
        , Attributes.d "M225.5 0v0c0 16.845 13.655 30.5 30.5 30.5v0"
        , Attributes.class "outline_2"
        ]
        []
    , path
        [ Attributes.fill "#BCA9A9"
        , Attributes.d "M256 188v36c-17.673 0-32 14.327-32 32h-36v-36c0-17.673 14.327-32 32-32h36z"
        , Attributes.class "sidewalk"
        ]
        []
    , path
        [ Attributes.fill "#BCA9A9"
        , Attributes.d "M32 256V0h36v256H32z"
        , Attributes.class "sidewalk_2"
        ]
        []
    , path
        [ Attributes.fill "#BCA9A9"
        , Attributes.d "M188 0h36c0 17.673 14.327 32 32 32v36h-36c-17.673 0-32-14.327-32-32V0z"
        , Attributes.class "sidewalk_3"
        ]
        []
    , path
        [ Attributes.stroke "#302525"
        , Attributes.strokeWidth "3"
        , Attributes.fill "none"
        , Attributes.d "M30.5 0v256"
        , Attributes.class "outline_3"
        ]
        []
    ]


intersectionTDown =
    [ path
        [ Attributes.fill "#785E5E"
        , Attributes.d "M68 256h120v-36c0-17.673 14.327-32 32-32h36V68H0v120h36c17.673 0 32 14.327 32 32v36z"
        , Attributes.class "road"
        ]
        []
    , Svg.g
        [ Attributes.class "markings"
        ]
        [ path
            [ Attributes.stroke "#F0F0DD"
            , Attributes.strokeWidth "6"
            , Attributes.d "M128 250v-32"
            , Attributes.class "markings_2"
            ]
            []
        , path
            [ Attributes.stroke "#F0F0DD"
            , Attributes.strokeWidth "6"
            , Attributes.d "M106 250v-32"
            , Attributes.class "markings_3"
            ]
            []
        , path
            [ Attributes.stroke "#F0F0DD"
            , Attributes.strokeWidth "6"
            , Attributes.d "M84 250v-32"
            , Attributes.class "markings_4"
            ]
            []
        , path
            [ Attributes.stroke "#F0F0DD"
            , Attributes.strokeWidth "6"
            , Attributes.d "M150 250v-32"
            , Attributes.class "markings_5"
            ]
            []
        , path
            [ Attributes.stroke "#F0F0DD"
            , Attributes.strokeWidth "6"
            , Attributes.d "M172 250v-32"
            , Attributes.class "markings_6"
            ]
            []
        , path
            [ Attributes.stroke "#F0F0DD"
            , Attributes.strokeWidth "6"
            , Attributes.d "M38 128H6"
            , Attributes.class "markings_7"
            ]
            []
        , path
            [ Attributes.stroke "#F0F0DD"
            , Attributes.strokeWidth "6"
            , Attributes.d "M38 150H6"
            , Attributes.class "markings_8"
            ]
            []
        , path
            [ Attributes.stroke "#F0F0DD"
            , Attributes.strokeWidth "6"
            , Attributes.d "M38 172H6"
            , Attributes.class "markings_9"
            ]
            []
        , path
            [ Attributes.stroke "#F0F0DD"
            , Attributes.strokeWidth "6"
            , Attributes.d "M38 106H6"
            , Attributes.class "markings_10"
            ]
            []
        , path
            [ Attributes.stroke "#F0F0DD"
            , Attributes.strokeWidth "6"
            , Attributes.d "M38 84H6"
            , Attributes.class "markings_11"
            ]
            []
        , path
            [ Attributes.stroke "#F0F0DD"
            , Attributes.strokeWidth "6"
            , Attributes.d "M250 128h-32"
            , Attributes.class "markings_12"
            ]
            []
        , path
            [ Attributes.stroke "#F0F0DD"
            , Attributes.strokeWidth "6"
            , Attributes.d "M250 150h-32"
            , Attributes.class "markings_13"
            ]
            []
        , path
            [ Attributes.stroke "#F0F0DD"
            , Attributes.strokeWidth "6"
            , Attributes.d "M250 172h-32"
            , Attributes.class "markings_14"
            ]
            []
        , path
            [ Attributes.stroke "#F0F0DD"
            , Attributes.strokeWidth "6"
            , Attributes.d "M250 106h-32"
            , Attributes.class "markings_15"
            ]
            []
        , path
            [ Attributes.stroke "#F0F0DD"
            , Attributes.strokeWidth "6"
            , Attributes.d "M250 84h-32"
            , Attributes.class "markings_16"
            ]
            []
        ]
    , path
        [ Attributes.stroke "#302525"
        , Attributes.strokeWidth "3"
        , Attributes.fill "none"
        , Attributes.d "M30.5 256v0c0-16.845-13.655-30.5-30.5-30.5v0"
        , Attributes.class "outline"
        ]
        []
    , path
        [ Attributes.stroke "#302525"
        , Attributes.strokeWidth "3"
        , Attributes.fill "none"
        , Attributes.d "M256 225.5v0c-16.845 0-30.5 13.655-30.5 30.5v0"
        , Attributes.class "outline_2"
        ]
        []
    , path
        [ Attributes.fill "#BCA9A9"
        , Attributes.d "M68 256H32c0-17.673-14.327-32-32-32v-36h36c17.673 0 32 14.327 32 32v36z"
        , Attributes.class "sidewalk"
        ]
        []
    , path
        [ Attributes.fill "#BCA9A9"
        , Attributes.d "M0 32h256v36H0V32z"
        , Attributes.class "sidewalk_2"
        ]
        []
    , path
        [ Attributes.fill "#BCA9A9"
        , Attributes.d "M256 188v36c-17.673 0-32 14.327-32 32h-36v-36c0-17.673 14.327-32 32-32h36z"
        , Attributes.class "sidewalk_3"
        ]
        []
    , path
        [ Attributes.stroke "#302525"
        , Attributes.strokeWidth "3"
        , Attributes.d "M256 30.5H0"
        , Attributes.fill "none"
        , Attributes.class "outline_3"
        ]
        []
    ]


intersectionCrossroads =
    [ path
        [ Attributes.fill "#785E5E"
        , Attributes.d "M188 0H68v36c0 17.673-14.327 32-32 32H0v120h36c17.673 0 32 14.327 32 32v36h120v-36c0-17.673 14.327-32 32-32h36V68h-36c-17.673 0-32-14.327-32-32V0z"
        , Attributes.class "road"
        ]
        []
    , Svg.g
        [ Attributes.class "markings"
        ]
        [ path
            [ Attributes.stroke "#F0F0DD"
            , Attributes.strokeWidth "6"
            , Attributes.d "M128 6v32"
            , Attributes.class "markings_2"
            ]
            []
        , path
            [ Attributes.stroke "#F0F0DD"
            , Attributes.strokeWidth "6"
            , Attributes.d "M150 6v32"
            , Attributes.class "markings_3"
            ]
            []
        , path
            [ Attributes.stroke "#F0F0DD"
            , Attributes.strokeWidth "6"
            , Attributes.d "M172 6v32"
            , Attributes.class "markings_4"
            ]
            []
        , path
            [ Attributes.stroke "#F0F0DD"
            , Attributes.strokeWidth "6"
            , Attributes.d "M106 6v32"
            , Attributes.class "markings_5"
            ]
            []
        , path
            [ Attributes.stroke "#F0F0DD"
            , Attributes.strokeWidth "6"
            , Attributes.d "M84 6v32"
            , Attributes.class "markings_6"
            ]
            []
        , path
            [ Attributes.stroke "#F0F0DD"
            , Attributes.strokeWidth "6"
            , Attributes.d "M128 218v32"
            , Attributes.class "markings_7"
            ]
            []
        , path
            [ Attributes.stroke "#F0F0DD"
            , Attributes.strokeWidth "6"
            , Attributes.d "M150 218v32"
            , Attributes.class "markings_8"
            ]
            []
        , path
            [ Attributes.stroke "#F0F0DD"
            , Attributes.strokeWidth "6"
            , Attributes.d "M172 218v32"
            , Attributes.class "markings_9"
            ]
            []
        , path
            [ Attributes.stroke "#F0F0DD"
            , Attributes.strokeWidth "6"
            , Attributes.d "M106 218v32"
            , Attributes.class "markings_10"
            ]
            []
        , path
            [ Attributes.stroke "#F0F0DD"
            , Attributes.strokeWidth "6"
            , Attributes.d "M84 218v32"
            , Attributes.class "markings_11"
            ]
            []
        , path
            [ Attributes.stroke "#F0F0DD"
            , Attributes.strokeWidth "6"
            , Attributes.d "M218 128h32"
            , Attributes.class "markings_12"
            ]
            []
        , path
            [ Attributes.stroke "#F0F0DD"
            , Attributes.strokeWidth "6"
            , Attributes.d "M218 106h32"
            , Attributes.class "markings_13"
            ]
            []
        , path
            [ Attributes.stroke "#F0F0DD"
            , Attributes.strokeWidth "6"
            , Attributes.d "M218 84h32"
            , Attributes.class "markings_14"
            ]
            []
        , path
            [ Attributes.stroke "#F0F0DD"
            , Attributes.strokeWidth "6"
            , Attributes.d "M218 150h32"
            , Attributes.class "markings_15"
            ]
            []
        , path
            [ Attributes.stroke "#F0F0DD"
            , Attributes.strokeWidth "6"
            , Attributes.d "M218 172h32"
            , Attributes.class "markings_16"
            ]
            []
        , path
            [ Attributes.stroke "#F0F0DD"
            , Attributes.strokeWidth "6"
            , Attributes.d "M6 128h32"
            , Attributes.class "markings_17"
            ]
            []
        , path
            [ Attributes.stroke "#F0F0DD"
            , Attributes.strokeWidth "6"
            , Attributes.d "M6 106h32"
            , Attributes.class "markings_18"
            ]
            []
        , path
            [ Attributes.stroke "#F0F0DD"
            , Attributes.strokeWidth "6"
            , Attributes.d "M6 84h32"
            , Attributes.class "markings_19"
            ]
            []
        , path
            [ Attributes.stroke "#F0F0DD"
            , Attributes.strokeWidth "6"
            , Attributes.d "M6 150h32"
            , Attributes.class "markings_20"
            ]
            []
        , path
            [ Attributes.stroke "#F0F0DD"
            , Attributes.strokeWidth "6"
            , Attributes.d "M6 172h32"
            , Attributes.class "markings_21"
            ]
            []
        ]
    , path
        [ Attributes.stroke "#302525"
        , Attributes.strokeWidth "3"
        , Attributes.fill "none"
        , Attributes.d "M225.5 0v0c0 16.648 13.351 30.219 29.996 30.492l.504.008"
        , Attributes.class "outline"
        ]
        []
    , path
        [ Attributes.stroke "#302525"
        , Attributes.strokeWidth "3"
        , Attributes.fill "none"
        , Attributes.d "M30.5 256v0c0-16.845-13.655-30.5-30.5-30.5v0"
        , Attributes.class "outline_2"
        ]
        []
    , path
        [ Attributes.stroke "#302525"
        , Attributes.strokeWidth "3"
        , Attributes.fill "none"
        , Attributes.d "M256 225.5v0c-16.845 0-30.5 13.655-30.5 30.5v0"
        , Attributes.class "outline_3"
        ]
        []
    , path
        [ Attributes.stroke "#302525"
        , Attributes.strokeWidth "3"
        , Attributes.fill "none"
        , Attributes.d "M0 30.5v0c16.845 0 30.5-13.655 30.5-30.5v0"
        , Attributes.class "outline_4"
        ]
        []
    , path
        [ Attributes.fill "#BCA9A9"
        , Attributes.d "M188 0h36c0 17.673 14.327 32 32 32v36h-36c-17.673 0-32-14.327-32-32V0z"
        , Attributes.class "sidewalk"
        ]
        []
    , path
        [ Attributes.fill "#BCA9A9"
        , Attributes.d "M68 256H32c0-17.673-14.327-32-32-32v-36h36c17.673 0 32 14.327 32 32v36z"
        , Attributes.class "sidewalk_2"
        ]
        []
    , path
        [ Attributes.fill "#BCA9A9"
        , Attributes.d "M0 68V32c17.673 0 32-14.327 32-32h36v36c0 17.673-14.327 32-32 32H0z"
        , Attributes.class "sidewalk_3"
        ]
        []
    , path
        [ Attributes.fill "#BCA9A9"
        , Attributes.d "M256 188v36c-17.673 0-32 14.327-32 32h-36v-36c0-17.673 14.327-32 32-32h36z"
        , Attributes.class "sidewalk_4"
        ]
        []
    ]
