module Data.Cars exposing
    ( CarMake
    , CarStyle
    , carAsset
    , hatchback
    , sedan
    , sedanGraphics
    , sedanGraphicsLazy
    , testCar
    , van
    )

import Color exposing (Color)
import Data.Colors as Colors
import Length exposing (Length)
import Model.Geometry exposing (LMShape2d)
import Point2d
import Polygon2d
import Quantity
import Svg exposing (Svg, path)
import Svg.Attributes as Attributes
import Svg.Lazy


type alias CarMake =
    { bodyColor : Color
    , accentColor : Color
    , length : Length
    , width : Length
    , shapeAtOrigin : LMShape2d
    , style : CarStyle
    }


type CarStyle
    = Sedan
    | Hatchback
    | Van


testCar : CarMake
testCar =
    sedan Colors.gray5 Colors.gray3


carAsset : CarMake -> ( Svg msg, String )
carAsset make =
    case make.style of
        Sedan ->
            ( sedanGraphicsLazy make.bodyColor make.accentColor, sedanViewBox )

        Hatchback ->
            ( hatchbackGraphicsLazy make.bodyColor make.accentColor, hatchbackViewBox )

        Van ->
            ( vanGraphicsLazy make.bodyColor make.accentColor, vanViewBox )


createCarMake : { style : CarStyle, bodyColor : Color, accentColor : Color, length : Length, width : Length } -> CarMake
createCarMake { style, bodyColor, accentColor, length, width } =
    let
        halfLength =
            Quantity.half length

        halfWidth =
            Quantity.half width

        p1 =
            Point2d.xy
                (Quantity.negate halfLength)
                (Quantity.negate halfWidth)

        p2 =
            Point2d.xy
                halfLength
                (Quantity.negate halfWidth)

        p3 =
            Point2d.xy
                halfLength
                halfWidth

        p4 =
            Point2d.xy
                (Quantity.negate halfLength)
                halfWidth

        shape =
            Polygon2d.singleLoop [ p1, p2, p3, p4 ]
    in
    { bodyColor = bodyColor
    , accentColor = accentColor
    , length = length
    , width = width
    , shapeAtOrigin = shape
    , style = style
    }


sedan : Color -> Color -> CarMake
sedan bodyColor accentColor =
    createCarMake
        { style = Sedan
        , length = Length.meters 4.6
        , width = Length.meters 2.36
        , bodyColor = bodyColor
        , accentColor = accentColor
        }


sedanViewBox : String
sedanViewBox =
    "0 0 120 62"


sedanGraphicsLazy : Color -> Color -> Svg msg
sedanGraphicsLazy =
    Svg.Lazy.lazy2 sedanGraphics


sedanGraphics : Color -> Color -> Svg msg
sedanGraphics bodyColor accentColor =
    let
        accentColorCSS =
            Color.toCssString accentColor
    in
    Svg.g
        []
        [ path
            [ Attributes.d "M2 31.0004C2 39.0096 3.18232 47.9091 4.08917 53.5572C4.63922 56.983 7.51065 59.5069 11.0023 59.6685C15.4307 59.8734 21.9854 60.0757 29.3708 59.9719C37.2663 59.8609 42.201 58.6792 50.4119 58.6792C60.646 58.6792 77.4256 59.8243 88.3483 59.9719C95.1001 60.0631 101.108 59.8802 105.29 59.6859C109.042 59.5117 112.335 57.2391 113.584 53.7235C115.515 48.2886 118 39.6207 118 31.0004C118 22.3799 115.515 13.7117 113.584 8.27682C112.335 4.76151 109.042 2.48902 105.29 2.31455C101.109 2.12009 95.1004 1.93684 88.3483 2.02809C77.4256 2.17569 60.646 3.3217 50.4119 3.3217C42.201 3.3217 37.2663 2.13907 29.3708 2.02809C21.9851 1.92427 15.4302 2.12682 11.0017 2.33205C7.51036 2.49385 4.63927 5.01758 4.08925 8.44316C3.18239 14.0912 2 22.9911 2 31.0004Z"
            , Attributes.fill (Color.toCssString bodyColor)
            ]
            []
        , path
            [ Attributes.fillRule "evenodd"
            , Attributes.clipRule "evenodd"
            , Attributes.d "M29.3475 3.64481C22.0035 3.54158 15.4828 3.74303 11.0776 3.94718C8.34951 4.0736 6.12524 6.03658 5.69798 8.69758C4.79523 14.3201 3.62903 23.12 3.62903 31.0004C3.62903 38.8805 4.79516 47.6802 5.6979 53.3027C6.12518 55.9639 8.34972 57.927 11.078 58.0533C15.4833 58.2572 22.0037 58.4583 29.3475 58.3551C33.0314 58.3033 36.0557 58.0145 39.146 57.7193C39.3029 57.7044 39.4599 57.6894 39.6172 57.6744C42.872 57.3643 46.2236 57.0622 50.4118 57.0622C55.5725 57.0622 62.3648 57.3506 69.1948 57.6549C69.6957 57.6772 70.1968 57.6996 70.6976 57.722C77.0469 58.0058 83.3378 58.2871 88.3703 58.3551C95.0812 58.4458 101.055 58.2639 105.213 58.0708C108.35 57.9251 111.035 56.0375 112.048 53.1857C113.955 47.8179 116.371 39.3516 116.371 31.0004C116.371 22.649 113.955 14.1824 112.048 8.8145C111.034 5.96298 108.351 4.07554 105.214 3.92967C101.056 3.7363 95.0815 3.55413 88.3703 3.64482C83.3373 3.71284 77.0457 3.99433 70.6957 4.27843C70.1956 4.30081 69.6951 4.3232 69.1948 4.34551C62.3649 4.65003 55.5725 4.93859 50.4118 4.93859C46.2235 4.93859 42.8719 4.63627 39.6171 4.32599C39.4599 4.31101 39.3031 4.29602 39.1463 4.28104C36.0559 3.98566 33.0315 3.6966 29.3475 3.64481ZM10.9256 0.716868C15.3772 0.510566 21.9665 0.306903 29.3937 0.411305C33.2143 0.46501 36.357 0.765514 39.4358 1.0599C39.6002 1.07562 39.7645 1.09133 39.9286 1.10698C43.1678 1.41576 46.389 1.70476 50.4118 1.70476C55.485 1.70476 62.1995 1.42031 69.0486 1.11493C69.5533 1.09243 70.0588 1.06981 70.5644 1.04718C76.8963 0.763848 83.2388 0.48004 88.326 0.411293C95.119 0.319493 101.161 0.503825 105.366 0.699383C109.733 0.902447 113.636 3.55999 115.121 7.73909C117.075 13.2409 119.629 22.1108 119.629 31.0004C119.629 39.8897 117.076 48.7593 115.121 54.2611C113.636 58.4406 109.733 61.0982 105.366 61.3011C101.16 61.4964 95.1187 61.6804 88.326 61.5886C83.2393 61.5199 76.8975 61.2363 70.5661 60.9532C70.06 60.9306 69.5539 60.908 69.0486 60.8854C62.1995 60.5803 55.485 60.2961 50.4118 60.2961C46.3889 60.2961 43.1677 60.5848 39.9285 60.8934C39.7643 60.909 39.6 60.9247 39.4355 60.9405C36.3568 61.2346 33.2141 61.5349 29.3937 61.5886C21.9668 61.693 15.3778 61.4896 10.9262 61.2836C6.67122 61.0866 3.15289 58.0021 2.48007 53.8116C1.56912 48.138 0.370605 39.1386 0.370605 31.0004C0.370605 22.862 1.56919 13.8623 2.48015 8.18869C3.15293 3.99853 6.67086 0.914044 10.9256 0.716868Z"
            , Attributes.fill accentColorCSS
            ]
            []
        , path
            [ Attributes.d "M109.528 11.7016C108.876 8.79114 108.225 4.58716 108.225 4.58716H110.18L113.112 7.82099L114.741 16.8757C114.741 16.8757 110.18 14.612 109.528 11.7016Z"
            , Attributes.fill Colors.gray7CSS
            ]
            []
        , path
            [ Attributes.fillRule "evenodd"
            , Attributes.clipRule "evenodd"
            , Attributes.d "M107.845 4.26367H110.325L113.417 7.67317L115.176 17.453L114.596 17.1649L114.741 16.8756C114.596 17.1649 114.596 17.1649 114.596 17.1649L114.591 17.1627L114.58 17.1572C114.571 17.1525 114.557 17.1456 114.54 17.1365C114.505 17.1184 114.454 17.0919 114.389 17.0572C114.26 16.9878 114.075 16.8859 113.852 16.7546C113.407 16.4923 112.805 16.1113 112.181 15.6366C110.952 14.7013 109.563 13.3472 109.21 11.7716C108.882 10.3085 108.555 8.52368 108.311 7.10749C108.188 6.39878 108.086 5.78122 108.015 5.34078C107.979 5.12055 107.951 4.94456 107.932 4.82356L107.91 4.68463L107.902 4.63629L107.845 4.26367ZM108.605 4.91044C108.62 5.00424 108.638 5.11408 108.658 5.23803C108.729 5.67678 108.831 6.29212 108.953 6.99821C109.197 8.41163 109.522 10.184 109.846 11.6313C110.145 12.9662 111.363 14.1992 112.577 15.1233C113.175 15.5784 113.755 15.9452 114.185 16.1983C114.225 16.2219 114.263 16.2444 114.301 16.266L112.808 7.96861L110.034 4.91044H108.605Z"
            , Attributes.fill accentColorCSS
            ]
            []
        , path
            [ Attributes.d "M109.528 50.5074C108.876 53.4178 108.225 57.6218 108.225 57.6218H110.18L113.112 54.388L114.741 45.3333C114.741 45.3333 110.18 47.597 109.528 50.5074Z"
            , Attributes.fill Colors.gray7CSS
            ]
            []
        , path
            [ Attributes.fillRule "evenodd"
            , Attributes.clipRule "evenodd"
            , Attributes.d "M107.845 57.9453H110.325L113.417 54.5358L115.176 44.756L114.596 45.0441L114.741 45.3334C114.596 45.0441 114.596 45.0441 114.596 45.0441L114.591 45.0463L114.58 45.0518C114.571 45.0565 114.557 45.0634 114.54 45.0725C114.505 45.0905 114.454 45.1171 114.389 45.1518C114.26 45.2212 114.075 45.3231 113.852 45.4544C113.407 45.7167 112.805 46.0977 112.181 46.5724C110.952 47.5077 109.563 48.8618 109.21 50.4373C108.882 51.9005 108.555 53.6853 108.311 55.1015C108.188 55.8102 108.086 56.4278 108.015 56.8682C107.979 57.0884 107.951 57.2644 107.932 57.3854L107.91 57.5244L107.902 57.5727L107.845 57.9453ZM108.605 57.2985C108.62 57.2047 108.638 57.0949 108.658 56.971C108.729 56.5322 108.831 55.9169 108.953 55.2108C109.197 53.7974 109.522 52.025 109.846 50.5777C110.145 49.2428 111.363 48.0098 112.577 47.0857C113.175 46.6306 113.755 46.2638 114.185 46.0107C114.225 45.9871 114.263 45.9646 114.301 45.943L112.808 54.2404L110.034 57.2985H108.605Z"
            , Attributes.fill accentColorCSS
            ]
            []
        , path
            [ Attributes.fillRule "evenodd"
            , Attributes.clipRule "evenodd"
            , Attributes.d "M29.3475 3.64481C22.0035 3.54158 15.4828 3.74303 11.0776 3.94718C8.34951 4.0736 6.12524 6.03658 5.69798 8.69758C4.79523 14.3201 3.62903 23.12 3.62903 31.0004C3.62903 38.8805 4.79516 47.6802 5.6979 53.3027C6.12518 55.9639 8.34972 57.927 11.078 58.0533C15.4833 58.2572 22.0037 58.4583 29.3475 58.3551C33.0314 58.3033 36.0557 58.0145 39.146 57.7193C39.3029 57.7044 39.4599 57.6894 39.6172 57.6744C42.872 57.3643 46.2236 57.0622 50.4118 57.0622C55.5725 57.0622 62.3648 57.3506 69.1948 57.6549C69.6957 57.6772 70.1968 57.6996 70.6976 57.722C77.0469 58.0058 83.3378 58.2871 88.3703 58.3551C95.0812 58.4458 101.055 58.2639 105.213 58.0708C108.35 57.9251 111.035 56.0375 112.048 53.1857C113.955 47.8178 116.371 39.3516 116.371 31.0004C116.371 22.649 113.955 14.1824 112.048 8.8145C111.034 5.96298 108.351 4.07554 105.214 3.92967C101.056 3.7363 95.0815 3.55413 88.3703 3.64482C83.3373 3.71284 77.0457 3.99433 70.6957 4.27843C70.1956 4.30081 69.6951 4.3232 69.1948 4.34551C62.3649 4.65003 55.5725 4.93859 50.4118 4.93859C46.2235 4.93859 42.8719 4.63627 39.6171 4.32599C39.4599 4.31101 39.3031 4.29602 39.1463 4.28104C36.0559 3.98566 33.0315 3.6966 29.3475 3.64481ZM10.9256 0.716868C15.3772 0.510566 21.9665 0.306903 29.3937 0.411305C33.2143 0.46501 36.357 0.765514 39.4358 1.0599C39.6002 1.07562 39.7645 1.09133 39.9286 1.10698C43.1678 1.41576 46.389 1.70476 50.4118 1.70476C55.485 1.70476 62.1995 1.42031 69.0486 1.11493C69.5533 1.09243 70.0588 1.06981 70.5644 1.04718C76.8963 0.763848 83.2388 0.48004 88.326 0.411293C95.119 0.319493 101.161 0.503825 105.366 0.699383C109.733 0.902447 113.636 3.55999 115.121 7.73909C117.075 13.2409 119.629 22.1108 119.629 31.0004C119.629 39.8897 117.076 48.7593 115.121 54.2611C113.636 58.4406 109.733 61.0982 105.366 61.3011C101.16 61.4964 95.1187 61.6804 88.326 61.5886C83.2393 61.5199 76.8975 61.2363 70.5661 60.9532C70.06 60.9306 69.5539 60.908 69.0486 60.8854C62.1995 60.5803 55.485 60.2961 50.4118 60.2961C46.3889 60.2961 43.1677 60.5848 39.9285 60.8934C39.7643 60.909 39.6 60.9247 39.4355 60.9405C36.3568 61.2346 33.2141 61.5349 29.3937 61.5886C21.9668 61.693 15.3778 61.4896 10.9262 61.2836C6.67122 61.0866 3.15289 58.0021 2.48007 53.8116C1.56912 48.138 0.370605 39.1386 0.370605 31.0004C0.370605 22.862 1.56919 13.8623 2.48015 8.18869C3.15293 3.99853 6.67086 0.914044 10.9256 0.716868Z"
            , Attributes.fill Colors.gray1CSS
            ]
            []
        , path
            [ Attributes.d "M60.515 51.5081L72.1865 52.7892C73.8954 52.9768 75.4532 51.8192 75.7174 50.1331C76.4116 45.7034 77.5956 37.293 77.5956 31.4561C77.5956 25.6243 76.4137 16.9008 75.7192 12.2786C75.457 10.5332 73.8158 9.35263 72.0574 9.61446L60.4368 11.3447C59.4475 11.492 58.7293 12.3508 58.7692 13.3427C58.9149 16.9673 59.2299 25.4885 59.2299 31.4561C59.2299 37.3862 58.9189 45.8376 58.7719 49.5004C58.731 50.5212 59.4918 51.3958 60.515 51.5081Z"
            , Attributes.fill Colors.gray7CSS
            ]
            []
        , path
            [ Attributes.fillRule "evenodd"
            , Attributes.clipRule "evenodd"
            , Attributes.d "M72.0433 54.0747L60.3719 52.7936C58.6712 52.6069 57.4015 51.1508 57.4698 49.4488C57.6168 45.7844 57.9267 37.3569 57.9267 31.456C57.9267 25.5175 57.6128 17.0205 57.467 13.3942C57.4005 11.7403 58.5993 10.3101 60.2436 10.0653L71.8642 8.33508C74.3203 7.96939 76.6379 9.62071 77.0085 12.0877C77.7016 16.7007 78.8991 25.5147 78.8991 31.456C78.8991 37.4058 77.6986 45.9086 77.0055 50.3317C76.6317 52.7168 74.4301 54.3367 72.0433 54.0747ZM72.1866 52.789L60.5151 51.5079C59.492 51.3956 58.7311 50.5211 58.7721 49.5002C58.9191 45.8374 59.2301 37.386 59.2301 31.456C59.2301 25.4883 58.9151 16.9672 58.7693 13.3426C58.7294 12.3506 59.4477 11.4918 60.4369 11.3445L72.0575 9.6143C73.816 9.35248 75.4572 10.533 75.7194 12.2784C76.4138 16.9007 77.5957 25.6242 77.5957 31.456C77.5957 37.2928 76.4118 45.7033 75.7176 50.1329C75.4533 51.819 73.8955 52.9766 72.1866 52.789Z"
            , Attributes.fill accentColorCSS
            ]
            []
        , path
            [ Attributes.d "M39.1001 49.0173L29.4475 49.1945C27.9285 49.2224 26.5948 48.2086 26.2712 46.7355C25.4792 43.1297 24.1571 36.3679 24.1571 31.4099C24.1571 26.4605 25.4746 19.4426 26.2671 15.669C26.5897 14.1327 28.0025 13.0891 29.5811 13.1795L39.1798 13.7292C40.2457 13.7902 41.0658 14.6851 41.0254 15.744C40.895 19.1676 40.6412 26.4646 40.6412 31.4099C40.6412 36.3193 40.8914 43.5467 41.0226 47.0009C41.0639 48.0894 40.1975 48.9971 39.1001 49.0173Z"
            , Attributes.fill Colors.gray7CSS
            ]
            []
        , path
            [ Attributes.fillRule "evenodd"
            , Attributes.clipRule "evenodd"
            , Attributes.d "M29.4719 50.4876L39.1246 50.3104C40.9498 50.2769 42.3943 48.7657 42.3253 46.952C42.194 43.4958 41.9449 36.2914 41.9449 31.4097C41.9449 26.4923 42.1977 19.2184 42.3282 15.7926C42.3954 14.0294 41.0291 12.5392 39.2552 12.4376L29.6565 11.888C27.4525 11.7617 25.4497 13.2228 24.9915 15.4049C24.2006 19.1707 22.854 26.31 22.854 31.4097C22.854 36.523 24.2069 43.4088 24.9981 47.0108C25.4584 49.1065 27.3515 50.5265 29.4719 50.4876ZM29.4478 49.1943L39.1005 49.0171C40.1978 48.9969 41.0643 48.0892 41.0229 47.0007C40.8917 43.5465 40.6416 36.3191 40.6416 31.4097C40.6416 26.4644 40.8953 19.1674 41.0258 15.7438C41.0661 14.6849 40.246 13.79 39.1801 13.729L29.5815 13.1793C28.0028 13.0889 26.59 14.1326 26.2674 15.6689C25.4749 19.4424 24.1574 26.4603 24.1574 31.4097C24.1574 36.3677 25.4795 43.1295 26.2716 46.7353C26.5951 48.2085 27.9289 49.2222 29.4478 49.1943Z"
            , Attributes.fill accentColorCSS
            ]
            []
        ]


hatchback : Color -> Color -> CarMake
hatchback bodyColor accentColor =
    createCarMake
        { style = Hatchback
        , length = Length.meters 4.3
        , width = Length.meters 2.5
        , bodyColor = bodyColor
        , accentColor = accentColor
        }


hatchbackViewBox : String
hatchbackViewBox =
    "0 0 112 66"


hatchbackGraphicsLazy : Color -> Color -> Svg msg
hatchbackGraphicsLazy =
    Svg.Lazy.lazy2 hatchbackGraphics


hatchbackGraphics : Color -> Color -> Svg msg
hatchbackGraphics bodyColor accentColor =
    let
        accentColorCSS =
            Color.toCssString accentColor
    in
    Svg.g
        []
        [ path
            [ Attributes.d "M2 33.0005C2 41.6914 3.59392 51.3846 4.74303 57.2436C5.38382 60.5108 8.05525 62.9252 11.364 63.2238C15.9465 63.6374 23.014 64.0961 31.025 63.9824C39.2029 63.8663 44.3142 63.3067 52.8189 63.3067C63.4191 63.3067 67.299 63.828 78.6125 63.9824C86.0081 64.0833 92.542 63.6546 96.8884 63.2536C100.38 62.9315 103.393 60.7741 104.689 57.5084C106.917 51.8918 110 42.4189 110 33.0005C110 23.5817 106.917 14.1085 104.689 8.49197C103.393 5.22658 100.38 3.06926 96.889 2.74697C92.5427 2.34575 86.0085 1.91666 78.6125 2.01759C67.299 2.172 63.4191 2.69423 52.8189 2.69423C44.3142 2.69423 39.2029 2.13369 31.025 2.01759C23.0136 1.90386 15.9459 2.36297 11.3635 2.7768C8.05498 3.07558 5.38388 5.48991 4.74312 8.75687C3.594 14.6158 2 24.3093 2 33.0005Z"
            , Attributes.fill (Color.toCssString bodyColor)
            ]
            []
        , path
            [ Attributes.d "M101.225 12.8131C100.55 9.76864 99.875 5.37109 99.875 5.37109H101.9L104.938 8.75382L106.625 18.2255C106.625 18.2255 101.9 15.8575 101.225 12.8131Z"
            , Attributes.fill Colors.gray7CSS
            ]
            []
        , path
            [ Attributes.fillRule "evenodd"
            , Attributes.clipRule "evenodd"
            , Attributes.d "M99.4814 5.03271H102.05L105.253 8.59919L107.075 18.8293L106.474 18.5279L106.625 18.2253C106.474 18.5279 106.474 18.528 106.474 18.5279L106.469 18.5256L106.458 18.5199C106.448 18.5149 106.434 18.5077 106.416 18.4983C106.38 18.4794 106.327 18.4515 106.26 18.4153C106.126 18.3427 105.935 18.2361 105.704 18.0988C105.242 17.8244 104.619 17.4258 103.973 16.9293C102.699 15.9509 101.261 14.5345 100.895 12.8864C100.556 11.3559 100.217 9.48886 99.9641 8.00747C99.8373 7.26613 99.7316 6.62014 99.6576 6.15942C99.6207 5.92904 99.5916 5.74495 99.5717 5.61838L99.5491 5.47306L99.5413 5.42249L99.4814 5.03271ZM100.269 5.70926C100.285 5.80738 100.303 5.92228 100.324 6.05193C100.398 6.51089 100.503 7.15456 100.629 7.89316C100.882 9.37165 101.219 11.2256 101.554 12.7396C101.864 14.1359 103.125 15.4257 104.383 16.3924C105.003 16.8684 105.603 17.2521 106.048 17.5168C106.09 17.5415 106.13 17.5651 106.168 17.5876L104.622 8.90824L101.75 5.70926H100.269Z"
            , Attributes.fill accentColorCSS
            ]
            []
        , path
            [ Attributes.d "M101.225 53.4057C100.55 56.4501 99.875 60.8477 99.875 60.8477H101.9L104.938 57.4649L106.625 47.9933C106.625 47.9933 101.9 50.3612 101.225 53.4057Z"
            , Attributes.fill Colors.gray7CSS
            ]
            []
        , path
            [ Attributes.fillRule "evenodd"
            , Attributes.clipRule "evenodd"
            , Attributes.d "M99.4814 61.186H102.05L105.253 57.6196L107.075 47.3895L106.474 47.6908L106.625 47.9934C106.474 47.6908 106.474 47.6908 106.474 47.6908L106.469 47.6931L106.458 47.6988C106.448 47.7038 106.434 47.711 106.416 47.7205C106.38 47.7394 106.327 47.7672 106.26 47.8035C106.126 47.8761 105.935 47.9827 105.704 48.12C105.242 48.3944 104.619 48.7929 103.973 49.2895C102.699 50.2678 101.261 51.6843 100.895 53.3324C100.556 54.8629 100.217 56.7299 99.9641 58.2113C99.8373 58.9526 99.7316 59.5986 99.6576 60.0593C99.6207 60.2897 99.5916 60.4738 99.5717 60.6004L99.5491 60.7457L99.5413 60.7963L99.4814 61.186ZM100.269 60.5095C100.285 60.4114 100.303 60.2965 100.324 60.1668C100.398 59.7079 100.503 59.0642 100.629 58.3256C100.882 56.8471 101.219 54.9931 101.554 53.4791C101.864 52.0828 103.125 50.7931 104.383 49.8264C105.003 49.3504 105.603 48.9667 106.048 48.7019C106.09 48.6773 106.13 48.6537 106.168 48.6311L104.622 57.3105L101.75 60.5095H100.269Z"
            , Attributes.fill accentColorCSS
            ]
            []
        , path
            [ Attributes.fillRule "evenodd"
            , Attributes.clipRule "evenodd"
            , Attributes.d "M31.0011 3.70874C23.0641 3.59607 16.0574 4.05107 11.5149 4.46129C8.95415 4.69255 6.89521 6.55275 6.39893 9.08308C5.2556 14.9125 3.6875 24.4768 3.6875 33.0004C3.6875 41.5238 5.25551 51.0878 6.39884 56.9173C6.89515 59.4478 8.95434 61.3081 11.5153 61.5392C16.0578 61.9492 23.0644 62.4038 31.0011 62.2912C34.6497 62.2394 37.6805 62.0987 40.7061 61.9584C41.0446 61.9427 41.383 61.927 41.7222 61.9114C45.0925 61.7566 48.5272 61.6153 52.8189 61.6153C58.1516 61.6153 61.7971 61.7466 65.5161 61.8966C65.8338 61.9094 66.152 61.9224 66.4718 61.9354C69.8879 62.0744 73.4865 62.2209 78.6355 62.2911C85.9562 62.3911 92.4286 61.9665 96.7337 61.5693C99.6248 61.3026 102.072 59.5282 103.121 56.8834C105.322 51.3357 108.312 42.0993 108.312 33.0004C108.312 23.9013 105.321 14.6645 103.121 9.11688C102.071 6.47234 99.625 4.69802 96.7343 4.43116C92.4291 4.03374 85.9566 3.60884 78.6355 3.70876C73.4854 3.77904 69.8864 3.92574 66.4696 4.06501C66.1506 4.07801 65.8332 4.09095 65.5162 4.10376C61.7972 4.25401 58.1516 4.38554 52.8189 4.38554C48.5272 4.38554 45.0924 4.24396 41.7221 4.08899C41.3834 4.07342 41.0456 4.05772 40.7077 4.04201C37.6816 3.9014 34.6503 3.76055 31.0011 3.70874ZM11.212 1.09221C15.8345 0.67477 22.9632 0.211571 31.0489 0.326357C34.7551 0.37897 37.8388 0.522301 40.8624 0.662837C41.201 0.678575 41.5389 0.694278 41.8767 0.709815C45.2327 0.864132 48.6059 1.00282 52.8189 1.00282C58.0863 1.00282 61.6808 0.873239 65.3802 0.723777C65.697 0.710978 66.0146 0.69803 66.3342 0.685005C69.7535 0.545617 73.3926 0.39727 78.5895 0.326344C86.0604 0.224383 92.6562 0.657663 97.0438 1.06269C101.136 1.44042 104.715 3.98075 106.257 7.86697C108.512 13.5524 111.688 23.2621 111.688 33.0004C111.688 42.7385 108.512 52.4478 106.257 58.1333C104.715 62.0199 101.135 64.5603 97.0431 64.9378C92.6555 65.3426 86.06 65.7755 78.5895 65.6736C73.3938 65.6026 69.7551 65.4545 66.3364 65.3154C66.0162 65.3024 65.6979 65.2894 65.3804 65.2766C61.6809 65.1274 58.0864 64.998 52.8189 64.998C48.6059 64.998 45.2326 65.1365 41.8766 65.2906C41.5381 65.3061 41.1997 65.3218 40.8604 65.3376C37.8374 65.4779 34.7543 65.6209 31.0489 65.6735C22.9635 65.7883 15.8351 65.3255 11.2127 64.9083C7.15616 64.5422 3.8725 61.5737 3.08722 57.5698C1.93232 51.6813 0.3125 41.8589 0.3125 33.0004C0.3125 24.1417 1.93241 14.319 3.08731 8.43058C3.87255 4.42698 7.15581 1.45852 11.212 1.09221Z"
            , Attributes.fill Colors.gray1CSS
            ]
            []
        , path
            [ Attributes.d "M56.7906 55.647L69.2255 56.0427C70.7319 56.0906 72.0843 55.1351 72.5032 53.684C73.7734 49.2843 76.2501 39.8823 76.2501 33.4594C76.2501 27.0056 73.7496 17.1664 72.485 12.6031C72.0771 11.1314 70.713 10.1522 69.1898 10.2007L56.782 10.5955C55.6229 10.6324 54.7298 11.6285 54.8192 12.7873C55.1557 17.1496 55.8774 27.2309 55.8774 33.4594C55.8774 39.6377 55.1673 49.2154 54.8274 53.446C54.734 54.6077 55.6283 55.61 56.7906 55.647Z"
            , Attributes.fill Colors.gray7CSS
            ]
            []
        , path
            [ Attributes.fillRule "evenodd"
            , Attributes.clipRule "evenodd"
            , Attributes.d "M69.1826 57.3951L56.7477 56.9995C54.815 56.938 53.3264 55.2703 53.4817 53.3374C53.8222 49.0995 54.5274 39.5749 54.5274 33.4594C54.5274 27.2919 53.8104 17.2629 53.4732 12.8917C53.3243 10.9618 54.8127 9.30446 56.7391 9.24316L69.1469 8.84835C71.2753 8.78062 73.2068 10.1521 73.7857 12.2411C75.0429 16.7777 77.6001 26.792 77.6001 33.4594C77.6001 40.1024 75.0639 49.6823 73.8 54.0602C73.2042 56.1239 71.2861 57.4621 69.1826 57.3951ZM69.2254 56.0427L56.7906 55.647C55.6283 55.6101 54.734 54.6078 54.8274 53.446C55.1673 49.2154 55.8774 39.6377 55.8774 33.4594C55.8774 27.2309 55.1557 17.1497 54.8191 12.7874C54.7297 11.6285 55.6229 10.6324 56.7819 10.5956L69.1897 10.2008C70.713 10.1523 72.0771 11.1314 72.485 12.6032C73.7496 17.1665 76.2501 27.0057 76.2501 33.4594C76.2501 39.8823 73.7734 49.2844 72.5032 53.684C72.0843 55.1351 70.7319 56.0907 69.2254 56.0427Z"
            , Attributes.fill accentColorCSS
            ]
            []
        , path
            [ Attributes.d "M27.881 53.5266L17.9056 52.3397C16.5124 52.174 15.3683 51.1635 15.0658 49.7903C14.2573 46.1201 12.7998 38.7615 12.7998 33.411C12.7998 28.0165 14.2814 20.2727 15.0856 16.4668C15.3776 15.0849 16.518 14.0614 17.9164 13.8859L27.877 12.6356C29.1521 12.4755 30.2536 13.5258 30.1572 14.8102C29.8295 19.1782 29.1987 28.2572 29.1987 33.411C29.1987 38.5217 29.819 47.1274 30.1489 51.3426C30.2492 52.6247 29.1552 53.6782 27.881 53.5266Z"
            , Attributes.fill Colors.gray7CSS
            ]
            []
        , path
            [ Attributes.fillRule "evenodd"
            , Attributes.clipRule "evenodd"
            , Attributes.d "M17.7465 53.6835L27.7218 54.8703C29.8412 55.1225 31.6617 53.37 31.4948 51.2369C31.1641 47.0113 30.5487 38.4595 30.5487 33.4111C30.5487 28.3175 31.1749 19.2915 31.5034 14.9117C31.6637 12.7743 29.8306 11.0267 27.7092 11.293L17.7486 12.5433C15.7958 12.7885 14.1795 14.2244 13.7649 16.1865C12.963 19.9814 11.4498 27.8558 11.4498 33.4111C11.4498 38.9268 12.9404 46.418 13.7475 50.0821C14.1775 52.0339 15.8 53.4519 17.7465 53.6835ZM17.9056 52.3398L27.881 53.5266C29.1551 53.6782 30.2492 52.6248 30.1489 51.3427C29.819 47.1274 29.1987 38.5217 29.1987 33.4111C29.1987 28.2572 29.8295 19.1783 30.1572 14.8102C30.2535 13.5258 29.1521 12.4756 27.877 12.6356L17.9164 13.8859C16.518 14.0615 15.3776 15.085 15.0856 16.4668C14.2814 20.2728 12.7998 28.0165 12.7998 33.4111C12.7998 38.7615 14.2573 46.1201 15.0658 49.7904C15.3683 51.1636 16.5124 52.174 17.9056 52.3398Z"
            , Attributes.fill accentColorCSS
            ]
            []
        ]


van : Color -> Color -> CarMake
van bodyColor accentColor =
    createCarMake
        { style = Van
        , length = Length.meters 5
        , width = Length.meters 2.7
        , bodyColor = bodyColor
        , accentColor = accentColor
        }


vanViewBox : String
vanViewBox =
    "0 0 128 69"


vanGraphicsLazy : Color -> Color -> Svg msg
vanGraphicsLazy =
    Svg.Lazy.lazy2 vanGraphics


vanGraphics : Color -> Color -> Svg msg
vanGraphics bodyColor accentColor =
    let
        accentColorCSS =
            Color.toCssString accentColor
    in
    Svg.g
        []
        [ path
            [ Attributes.d "M2.5 34.5C2.5 43.6337 4.01723 55.5289 4.91428 61.7637C5.26866 64.2267 7.34053 66.0334 9.80994 66.0763C19.6414 66.2471 44.4191 66.6456 61.6215 66.6456C78.4883 66.6456 102.638 66.2625 112.841 66.0866C115.809 66.0354 118.463 64.2629 119.521 61.4676C121.831 55.3606 125.5 44.1427 125.5 34.5C125.5 24.8574 121.831 13.6395 119.521 7.53246C118.463 4.73718 115.809 2.96472 112.841 2.91353C102.638 2.73757 78.4883 2.35449 61.6215 2.35449C44.4191 2.35449 19.6414 2.75296 9.80994 2.92378C7.34054 2.96669 5.26866 4.77335 4.91428 7.23636C4.01723 13.4711 2.5 25.3664 2.5 34.5Z"
            , Attributes.fill (Color.toCssString bodyColor)
            ]
            []
        , path
            [ Attributes.d "M118.87 15.1327L121.723 15.7078L119.081 8.53861C118.014 5.64439 115.273 3.72412 112.209 3.72412L113.923 10.4157C114.537 12.8142 116.459 14.6469 118.87 15.1327Z"
            , Attributes.fill Colors.gray7CSS
            ]
            []
        , path
            [ Attributes.fillRule "evenodd"
            , Attributes.clipRule "evenodd"
            , Attributes.d "M121.723 15.7078L119.081 8.53861C118.092 5.85701 115.667 4.01152 112.879 3.75484C112.658 3.73448 112.435 3.72412 112.209 3.72412L113.923 10.4157C114.537 12.8142 116.459 14.6469 118.87 15.1327L121.723 15.7078ZM120.723 14.8479L118.48 8.76338C117.605 6.39006 115.497 4.73438 113.05 4.42287L114.543 10.2544C115.096 12.413 116.826 14.0625 118.995 14.4997L120.723 14.8479Z"
            , Attributes.fill accentColorCSS
            ]
            []
        , path
            [ Attributes.d "M118.87 53.8675L121.723 53.2924L119.081 60.4616C118.014 63.3559 115.273 65.2761 112.209 65.2761L113.923 58.5846C114.537 56.1861 116.459 54.3533 118.87 53.8675Z"
            , Attributes.fill Colors.gray7CSS
            ]
            []
        , path
            [ Attributes.fillRule "evenodd"
            , Attributes.clipRule "evenodd"
            , Attributes.d "M121.723 53.2924L119.081 60.4616C118.092 63.1432 115.667 64.9887 112.879 65.2454C112.658 65.2658 112.435 65.2761 112.209 65.2761L113.923 58.5846C114.537 56.1861 116.459 54.3533 118.87 53.8675L121.723 53.2924ZM120.723 54.1524L118.48 60.2369C117.605 62.6102 115.497 64.2659 113.05 64.5774L114.543 58.7459C115.096 56.5872 116.826 54.9377 118.995 54.5005L120.723 54.1524Z"
            , Attributes.fill accentColorCSS
            ]
            []
        , path
            [ Attributes.fillRule "evenodd"
            , Attributes.clipRule "evenodd"
            , Attributes.d "M61.6215 3.96823C44.4347 3.96823 19.6701 4.36644 9.83755 4.53728C8.13882 4.56679 6.7389 5.80228 6.49927 7.46779C5.60263 13.6998 4.10156 25.4935 4.10156 34.4999C4.10156 43.5063 5.60263 55.3 6.49927 61.532C6.7389 63.1975 8.13882 64.433 9.83755 64.4625C19.6701 64.6334 44.4347 65.0316 61.6215 65.0316C78.4728 65.0316 102.609 64.6488 112.813 64.4728C115.166 64.4322 117.214 63.0356 118.024 60.8927C120.326 54.8068 123.898 43.8259 123.898 34.4999C123.898 25.1739 120.326 14.193 118.024 8.10715C117.214 5.96418 115.166 4.56761 112.813 4.52703C102.609 4.35105 78.4728 3.96823 61.6215 3.96823ZM9.78233 1.31001C19.6127 1.13921 44.4034 0.740479 61.6215 0.740479C78.5038 0.740479 102.666 1.1238 112.868 1.29975C116.451 1.36155 119.713 3.5099 121.017 6.9575C123.335 13.0856 127.102 24.5407 127.102 34.4999C127.102 44.4591 123.335 55.9142 121.017 62.0423C119.713 65.4899 116.451 67.6383 112.868 67.7001C102.666 67.876 78.5038 68.2593 61.6215 68.2593C44.4034 68.2593 19.6127 67.8606 9.78233 67.6898C6.54225 67.6335 3.79841 65.2557 3.32929 61.9951C2.43183 55.7575 0.898438 43.7608 0.898438 34.4999C0.898438 25.239 2.43183 13.2423 3.32929 7.00466C3.79841 3.74414 6.54225 1.3663 9.78233 1.31001Z"
            , Attributes.fill Colors.gray1CSS
            ]
            []
        , path
            [ Attributes.d "M79.0547 59.8851L95.8438 61.0519C97.4965 61.1667 98.9607 59.9963 99.2011 58.3447C99.9702 53.0604 101.48 41.7675 101.48 34.5481C101.48 27.3288 99.9702 16.0359 99.2011 10.7516C98.9607 9.09992 97.4965 7.92954 95.8438 8.04439L79.0547 9.21116H13.8807C12.3066 9.21116 10.9685 10.3585 10.7429 11.9283C10.0439 16.7927 8.67775 27.1695 8.58594 34.5481C8.49481 41.872 9.5852 52.1876 10.1626 57.0886C10.3518 58.6946 11.7062 59.8851 13.3111 59.8851H79.0547Z"
            , Attributes.fill accentColorCSS
            ]
            []
        , path
            [ Attributes.fillRule "evenodd"
            , Attributes.clipRule "evenodd"
            , Attributes.d "M79.0988 58.594L95.932 59.7638C96.9274 59.833 97.7917 59.1309 97.9334 58.1573C98.7045 52.8595 100.199 41.6584 100.199 34.5481C100.199 27.4379 98.7045 16.2367 97.9334 10.939C97.7917 9.96536 96.9274 9.26326 95.932 9.33243L79.0988 10.5023H13.8807C12.9318 10.5023 12.1436 11.1899 12.0109 12.1133C11.3113 16.9823 9.95771 27.2812 9.86709 34.5643C9.77714 41.7932 10.857 52.031 11.4349 56.9364C11.5462 57.8805 12.343 58.594 13.3111 58.594H79.0988ZM99.2011 10.7516C99.9702 16.0359 101.48 27.3288 101.48 34.5481C101.48 41.7675 99.9702 53.0604 99.2011 58.3447C98.9607 59.9963 97.4965 61.1667 95.8438 61.0519L79.0547 59.8851H13.3111C11.7062 59.8851 10.3518 58.6946 10.1626 57.0886C9.5852 52.1876 8.49481 41.872 8.58594 34.5481C8.67775 27.1695 10.0439 16.7927 10.7429 11.9283C10.9685 10.3585 12.3066 9.21116 13.8807 9.21116H79.0547L95.8438 8.04439C97.4965 7.92954 98.9607 9.09992 99.2011 10.7516Z"
            , Attributes.fill accentColorCSS
            ]
            []
        , path
            [ Attributes.d "M81.016 58.7304L95.8814 59.7634C97.5184 59.8772 98.9723 58.7326 99.2267 57.0991C100.003 52.1128 101.48 41.6887 101.48 34.5482C101.48 27.4077 100.003 16.9837 99.2267 11.9973C98.9723 10.3638 97.5184 9.21922 95.8814 9.33298L81.016 10.3661C79.9401 10.4408 79.1346 11.3906 79.2252 12.4735C79.59 16.8307 80.414 27.4565 80.414 34.5482C80.414 41.6399 79.59 52.2657 79.2252 56.623C79.1346 57.7058 79.9401 58.6556 81.016 58.7304Z"
            , Attributes.fill Colors.gray7CSS
            ]
            []
        , path
            [ Attributes.fillRule "evenodd"
            , Attributes.clipRule "evenodd"
            , Attributes.d "M95.7929 61.0513L80.9274 60.0182C79.1354 59.8937 77.7975 58.3128 77.9481 56.5143C78.3134 52.151 79.1323 41.5774 79.1323 34.5481C79.1323 27.5187 78.3134 16.9452 77.9481 12.5818C77.7975 10.7833 79.1354 9.2024 80.9274 9.07787L95.7929 8.04478C98.0789 7.88592 100.132 9.48812 100.492 11.797C101.267 16.7748 102.761 27.293 102.761 34.5481C102.761 41.8031 101.267 52.3214 100.492 57.2991C100.132 59.608 98.0789 61.2102 95.7929 61.0513ZM95.881 59.7633L81.0155 58.7302C79.9397 58.6554 79.1341 57.7057 79.2248 56.6228C79.5896 52.2655 80.4136 41.6397 80.4136 34.5481C80.4136 27.4564 79.5896 16.8306 79.2248 12.4733C79.1341 11.3905 79.9397 10.4407 81.0155 10.3659L95.881 9.33282C97.518 9.21906 98.9719 10.3637 99.2263 11.9971C100.003 16.9835 101.48 27.4075 101.48 34.5481C101.48 41.6886 100.003 52.1126 99.2263 57.099C98.9719 58.7324 97.518 59.877 95.881 59.7633Z"
            , Attributes.fill accentColorCSS
            ]
            []
        ]
