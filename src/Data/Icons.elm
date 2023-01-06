module Data.Icons exposing (IconKind(..), chooseIcon)

import Data.Colors as Colors
import Html exposing (Html)
import Svg exposing (Svg, path, svg)
import Svg.Attributes as Attributes


type IconKind
    = NewGame
    | Pause
    | Resume
    | Back
    | Close
    | DebugPanel
    | CarDebug
    | LotDebug
    | GraphDebug
    | SpawnCar


chooseIcon : IconKind -> ( Html msg, Colors.Color, Colors.Color )
chooseIcon kind =
    case kind of
        NewGame ->
            ( iconNewGame, Colors.lightGreen, Colors.lightGreenDarker )

        Pause ->
            ( iconPause, Colors.darkBlue, Colors.darkBlueDarker )

        Resume ->
            ( iconResume, Colors.darkBlue, Colors.darkBlueDarker )

        Back ->
            ( iconBack, Colors.yellow, Colors.yellowDarker )

        Close ->
            ( iconClose, Colors.gray1, Colors.gray2 )

        DebugPanel ->
            ( iconToggleDebug, Colors.red, Colors.redDarker )

        CarDebug ->
            ( iconToggleCarDebug, Colors.gray5, Colors.gray6 )

        LotDebug ->
            ( Html.text "🏡", Colors.gray5, Colors.gray6 )

        GraphDebug ->
            ( iconToggleGraphDebug, Colors.gray5, Colors.gray6 )

        SpawnCar ->
            ( iconSpawnCar, Colors.gray5, Colors.gray6 )


iconNewGame : Svg msg
iconNewGame =
    svg
        [ Attributes.width "256"
        , Attributes.height "256"
        , Attributes.viewBox "0 0 256 256"
        , Attributes.fill "none"
        ]
        [ path
            [ Attributes.d "M207.5 28C207.5 24.4101 204.59 21.5 201 21.5C197.41 21.5 194.5 24.4101 194.5 28V48.5H174C170.41 48.5 167.5 51.4102 167.5 55C167.5 58.5899 170.41 61.5 174 61.5H194.5V82C194.5 85.5899 197.41 88.5 201 88.5C204.59 88.5 207.5 85.5899 207.5 82V61.5H228C231.59 61.5 234.5 58.5899 234.5 55C234.5 51.4102 231.59 48.5 228 48.5H207.5V28Z"
            , Attributes.fill "#F9F9E9"
            , Attributes.stroke "#3D3434"
            , Attributes.strokeWidth "3"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            ]
            []
        , path
            [ Attributes.d "M130 115.029H167.5V163.779H92.5002V139.404V115.029H130Z"
            , Attributes.fill "#F9F9E9"
            ]
            []
        , path
            [ Attributes.d "M91.1533 116.535H168.846"
            , Attributes.stroke "#3D3434"
            , Attributes.strokeWidth "3"
            ]
            []
        , path
            [ Attributes.d "M130 89.8843L170 115.557H90L130 89.8843Z"
            , Attributes.fill "#F9F9E9"
            ]
            []
        , Svg.rect
            [ Attributes.x "134.578"
            , Attributes.y "130.807"
            , Attributes.width "18.5385"
            , Attributes.height "18.5385"
            , Attributes.rx "2.5"
            , Attributes.fill "#F9F9E9"
            , Attributes.stroke "#3D3434"
            , Attributes.strokeWidth "3"
            ]
            []
        , path
            [ Attributes.d "M106.307 139.269H116.769C118.15 139.269 119.269 140.389 119.269 141.769V164.731H103.807V141.769C103.807 140.389 104.927 139.269 106.307 139.269Z"
            , Attributes.fill "#F9F9E9"
            , Attributes.stroke "#3D3434"
            , Attributes.strokeWidth "3"
            , Attributes.strokeLinecap "round"
            ]
            []
        , path
            [ Attributes.d "M133.629 91.251L167.396 113.978C168.086 114.443 168.5 115.22 168.5 116.052V162.231C168.5 163.611 167.381 164.731 166 164.731H94C92.6193 164.731 91.5 163.611 91.5 162.231V116.052C91.5 115.22 91.9139 114.443 92.6041 113.978L126.371 91.251C128.565 89.7741 131.435 89.7741 133.629 91.251Z"
            , Attributes.stroke "#3D3434"
            , Attributes.strokeWidth "3"
            ]
            []
        , path
            [ Attributes.d "M234.5 206C234.5 221.74 221.74 234.5 206 234.5L40 234.5C29.7827 234.5 21.5 226.217 21.5 216L21.5 50C21.5 34.2599 34.2599 21.5 50 21.5C65.7401 21.5 78.5 34.2599 78.5 50L78.5 171C78.5 174.59 81.4101 177.5 85 177.5L206 177.5C221.74 177.5 234.5 190.26 234.5 206Z"
            , Attributes.fill "#766565"
            , Attributes.stroke "#3D3434"
            , Attributes.strokeWidth "3"
            , Attributes.strokeLinecap "round"
            ]
            []
        , path
            [ Attributes.d "M50 66L50 52"
            , Attributes.stroke "#F9F9E9"
            , Attributes.strokeWidth "8"
            , Attributes.strokeLinecap "round"
            ]
            []
        , path
            [ Attributes.d "M50 100L50 86"
            , Attributes.stroke "#F9F9E9"
            , Attributes.strokeWidth "8"
            , Attributes.strokeLinecap "round"
            ]
            []
        , path
            [ Attributes.d "M50 134L50 120"
            , Attributes.stroke "#F9F9E9"
            , Attributes.strokeWidth "8"
            , Attributes.strokeLinecap "round"
            ]
            []
        , path
            [ Attributes.d "M50 168L50 154"
            , Attributes.stroke "#F9F9E9"
            , Attributes.strokeWidth "8"
            , Attributes.strokeLinecap "round"
            ]
            []
        , path
            [ Attributes.d "M190 206H204"
            , Attributes.stroke "#F9F9E9"
            , Attributes.strokeWidth "8"
            , Attributes.strokeLinecap "round"
            ]
            []
        , path
            [ Attributes.d "M156 206H170"
            , Attributes.stroke "#F9F9E9"
            , Attributes.strokeWidth "8"
            , Attributes.strokeLinecap "round"
            ]
            []
        , path
            [ Attributes.d "M122 206H136"
            , Attributes.stroke "#F9F9E9"
            , Attributes.strokeWidth "8"
            , Attributes.strokeLinecap "round"
            ]
            []
        , path
            [ Attributes.d "M88 206H102"
            , Attributes.stroke "#F9F9E9"
            , Attributes.strokeWidth "8"
            , Attributes.strokeLinecap "round"
            ]
            []
        ]


iconPause : Svg msg
iconPause =
    svg
        [ Attributes.width "256"
        , Attributes.height "256"
        , Attributes.viewBox "0 0 256 256"
        , Attributes.fill "none"
        ]
        [ path
            [ Attributes.d "M173.267 21.5C189.271 21.5 202.19 34.1949 202.19 49.7887L202.19 206.211C202.19 221.805 189.271 234.5 173.267 234.5C157.264 234.5 144.345 221.805 144.345 206.211L144.345 49.7887C144.345 34.1949 157.264 21.5 173.267 21.5Z"
            , Attributes.fill "#F9F9E9"
            , Attributes.stroke "#3D3434"
            , Attributes.strokeWidth "3"
            , Attributes.strokeLinecap "round"
            ]
            []
        , path
            [ Attributes.d "M82.4224 21.5C98.4258 21.5 111.345 34.1949 111.345 49.7887L111.345 206.211C111.345 221.805 98.4258 234.5 82.4224 234.5C66.419 234.5 53.4999 221.805 53.4999 206.211L53.4999 49.7887C53.4999 34.1949 66.419 21.5 82.4224 21.5Z"
            , Attributes.fill "#F9F9E9"
            , Attributes.stroke "#3D3434"
            , Attributes.strokeWidth "3"
            , Attributes.strokeLinecap "round"
            ]
            []
        ]


iconResume : Svg msg
iconResume =
    svg
        [ Attributes.width "256"
        , Attributes.height "256"
        , Attributes.viewBox "0 0 256 256"
        , Attributes.fill "none"
        ]
        [ path
            [ Attributes.d "M53.5 215.427V40.5871C53.5 25.2431 71.1035 16.5687 83.271 25.9169L194.842 111.636C204.366 118.953 204.501 133.265 195.117 140.761L83.5461 229.882C71.4308 239.559 53.5 230.933 53.5 215.427Z"
            , Attributes.fill "#F9F9E9"
            , Attributes.stroke "#3D3434"
            , Attributes.strokeWidth "3"
            ]
            []
        ]


iconBack : Svg msg
iconBack =
    svg
        [ Attributes.width "256"
        , Attributes.height "256"
        , Attributes.viewBox "0 0 256 256"
        , Attributes.fill "none"
        ]
        [ path
            [ Attributes.d "M187.052 29.9207C198.163 40.993 198.318 58.7114 187.493 69.499L133.91 122.896C131.361 125.436 131.361 129.564 133.91 132.104L187.493 185.501C198.318 196.289 198.163 214.007 187.052 225.079C175.939 236.154 158.15 236.309 147.322 225.518L71.0629 149.523C64.7302 143.213 60.4799 135.866 60.5002 127.523L60.5002 127.513C60.4692 119.164 65.0126 111.506 71.0616 105.478L147.322 29.4817C158.15 18.691 175.939 18.8457 187.052 29.9207ZM59.0008 127.519L59.0002 127.519L59.0008 127.519Z"
            , Attributes.fill "#F9F9E9"
            , Attributes.stroke "#3D3434"
            , Attributes.strokeWidth "3"
            , Attributes.strokeLinecap "round"
            ]
            []
        ]


iconClose : Svg msg
iconClose =
    svg
        [ Attributes.width "256"
        , Attributes.height "256"
        , Attributes.viewBox "0 0 256 256"
        , Attributes.fill "none"
        ]
        [ path
            [ Attributes.d "M225.729 30.6933C237.046 42.0094 237.204 60.1212 226.178 71.1477L173.921 123.404C171.383 125.942 171.383 130.058 173.921 132.596L226.178 184.853C237.204 195.879 237.046 213.991 225.729 225.307C214.413 236.623 196.301 236.782 185.275 225.755L133.019 173.499C130.48 170.961 126.365 170.961 123.826 173.499L71.5701 225.755C60.5436 236.782 42.4318 236.623 31.1157 225.307C19.7995 213.991 19.6409 195.879 30.6674 184.853L82.9237 132.596C85.4621 130.058 85.4621 125.942 82.9237 123.404L30.6675 71.1478C19.641 60.1213 19.7996 42.0094 31.1157 30.6933C42.4318 19.3772 60.5436 19.2186 71.5701 30.2451L123.826 82.5013C126.365 85.0397 130.48 85.0397 133.019 82.5013L185.275 30.2451C196.301 19.2186 214.413 19.3772 225.729 30.6933Z"
            , Attributes.fill "#F9F9E9"
            , Attributes.stroke "#3D3434"
            , Attributes.strokeWidth "3"
            , Attributes.strokeLinecap "round"
            ]
            []
        ]


iconToggleDebug : Svg msg
iconToggleDebug =
    svg
        [ Attributes.width "256"
        , Attributes.height "256"
        , Attributes.viewBox "0 0 256 256"
        , Attributes.fill "none"
        ]
        [ path
            [ Attributes.d "M197.5 145.493C197.5 182.45 166.435 212.5 128 212.5C89.5652 212.5 58.5 182.45 58.5 145.493C58.5 128.662 64.935 113.277 75.5759 101.5H180.424C191.065 113.277 197.5 128.662 197.5 145.493Z"
            , Attributes.fill "#F9F9E9"
            , Attributes.stroke "#3D3434"
            , Attributes.strokeWidth "3"
            ]
            []
        , path
            [ Attributes.d "M74.5217 92.5C75.3074 65.3429 97.3861 43.5 127.526 43.5C157.7 43.5 180.659 65.3741 181.477 92.5H74.5217Z"
            , Attributes.fill "#F9F9E9"
            , Attributes.stroke "#3D3434"
            , Attributes.strokeWidth "3"
            ]
            []
        , path
            [ Attributes.d "M209.2 201.893L209.2 201.893C209.89 202.58 211.236 203.233 212.776 203.354C214.273 203.471 215.789 203.076 216.96 201.902C218.13 200.728 218.521 199.211 218.399 197.715C218.273 196.175 217.614 194.832 216.926 194.147L202.741 180.048L202.741 180.048C202.051 179.361 200.705 178.708 199.165 178.587C197.668 178.469 196.152 178.865 194.982 180.039C193.811 181.213 193.42 182.73 193.542 184.226C193.668 185.765 194.327 187.108 195.015 187.793C195.015 187.793 195.015 187.793 195.015 187.794C195.015 187.794 195.016 187.794 195.016 187.794L209.2 201.893Z"
            , Attributes.fill "#F9F9E9"
            , Attributes.stroke "#3D3434"
            , Attributes.strokeWidth "3"
            , Attributes.strokeLinecap "round"
            ]
            []
        , path
            [ Attributes.d "M229.007 147.44L229.007 147.44C229.98 147.437 231.395 146.948 232.569 145.944C233.71 144.969 234.502 143.617 234.5 141.959C234.498 140.301 233.701 138.952 232.557 137.981C231.379 136.981 229.964 136.497 228.993 136.5L208.993 136.56L208.993 136.56C208.02 136.562 206.605 137.052 205.431 138.056C204.29 139.031 203.498 140.383 203.5 142.041C203.502 143.699 204.299 145.048 205.443 146.019C206.62 147.018 208.036 147.502 209.007 147.5C209.007 147.5 209.007 147.5 209.007 147.5C209.007 147.5 209.007 147.5 209.007 147.5L229.007 147.44Z"
            , Attributes.fill "#F9F9E9"
            , Attributes.stroke "#3D3434"
            , Attributes.strokeWidth "3"
            , Attributes.strokeLinecap "round"
            ]
            []
        , path
            [ Attributes.d "M212.893 89.7415L212.893 89.7411C213.58 89.0513 214.233 87.7047 214.354 86.1647C214.471 84.668 214.076 83.1523 212.902 81.9815C211.728 80.811 210.211 80.4199 208.715 80.5424C207.175 80.6684 205.832 81.3271 205.147 82.0156L191.048 96.1997L191.048 96.2001C190.361 96.8899 189.708 98.2365 189.587 99.7765C189.469 101.273 189.865 102.789 191.039 103.96C192.213 105.13 193.73 105.521 195.226 105.399C196.765 105.273 198.108 104.614 198.793 103.926C198.793 103.926 198.793 103.926 198.794 103.926C198.794 103.926 198.794 103.926 198.794 103.926L212.893 89.7415Z"
            , Attributes.fill "#F9F9E9"
            , Attributes.stroke "#3D3434"
            , Attributes.strokeWidth "3"
            , Attributes.strokeLinecap "round"
            ]
            []
        , path
            [ Attributes.d "M46.7415 201.893L46.7411 201.893C46.0513 202.58 44.7047 203.233 43.1647 203.354C41.668 203.471 40.1523 203.076 38.9815 201.902C37.811 200.728 37.4199 199.211 37.5424 197.715C37.6684 196.175 38.3271 194.832 39.0156 194.147L53.1997 180.048L53.2001 180.048C53.8899 179.361 55.2365 178.708 56.7765 178.587C58.2732 178.469 59.7889 178.865 60.9597 180.039C62.1302 181.213 62.5212 182.73 62.3988 184.226C62.2728 185.765 61.6144 187.108 60.926 187.793C60.9259 187.793 60.9258 187.793 60.9257 187.794C60.9257 187.794 60.9256 187.794 60.9256 187.794L46.7415 201.893Z"
            , Attributes.fill "#F9F9E9"
            , Attributes.stroke "#3D3434"
            , Attributes.strokeWidth "3"
            , Attributes.strokeLinecap "round"
            ]
            []
        , path
            [ Attributes.d "M26.9933 147.44L26.9927 147.44C26.0198 147.437 24.6054 146.948 23.431 145.944C22.2897 144.969 21.4977 143.617 21.5 141.959C21.5024 140.301 22.2986 138.952 23.443 137.981C24.6207 136.981 26.0361 136.497 27.0073 136.5L47.0067 136.56L47.0073 136.56C47.9803 136.562 49.3946 137.052 50.569 138.056C51.7104 139.031 52.5023 140.383 52.5 142.041C52.4976 143.699 51.7014 145.048 50.557 146.019C49.3795 147.018 47.9645 147.502 46.9933 147.5C46.9932 147.5 46.993 147.5 46.9929 147.5C46.9928 147.5 46.9928 147.5 46.9927 147.5L26.9933 147.44Z"
            , Attributes.fill "#F9F9E9"
            , Attributes.stroke "#3D3434"
            , Attributes.strokeWidth "3"
            , Attributes.strokeLinecap "round"
            ]
            []
        , path
            [ Attributes.d "M43.0482 89.7415L43.0478 89.7411C42.3616 89.0513 41.7079 87.7047 41.5871 86.1647C41.4697 84.668 41.8654 83.1523 43.0394 81.9815C44.2133 80.811 45.7304 80.4199 47.2264 80.5424C48.7659 80.6684 50.1089 81.3271 50.7939 82.0156L64.8932 96.1997L64.8936 96.2001C65.5798 96.8899 66.2334 98.2365 66.3543 99.7765C66.4717 101.273 66.076 102.789 64.9019 103.96C63.7281 105.13 62.211 105.521 60.715 105.399C59.1758 105.273 57.833 104.614 57.1479 103.926C57.1478 103.926 57.1477 103.926 57.1477 103.926C57.1476 103.926 57.1476 103.926 57.1475 103.926L43.0482 89.7415Z"
            , Attributes.fill "#F9F9E9"
            , Attributes.stroke "#3D3434"
            , Attributes.strokeWidth "3"
            , Attributes.strokeLinecap "round"
            ]
            []
        ]


iconToggleGraphDebug : Svg msg
iconToggleGraphDebug =
    svg
        [ Attributes.width "256"
        , Attributes.height "256"
        , Attributes.viewBox "0 0 256 256"
        , Attributes.fill "none"
        ]
        [ path
            [ Attributes.fillRule "evenodd"
            , Attributes.clipRule "evenodd"
            , Attributes.d "M215.446 40.5535L40.5537 128L38.0381 122.969L212.931 35.5224L215.446 40.5535Z"
            , Attributes.fill "#E68C4D"
            ]
            []
        , path
            [ Attributes.fillRule "evenodd"
            , Attributes.clipRule "evenodd"
            , Attributes.d "M215.447 215.446L40.5538 128L43.0693 122.969L217.962 210.415L215.447 215.446Z"
            , Attributes.fill "#E68C4D"
            ]
            []
        , path
            [ Attributes.fillRule "evenodd"
            , Attributes.clipRule "evenodd"
            , Attributes.d "M216 40L216 216H210L210 40H216Z"
            , Attributes.fill "#E68C4D"
            ]
            []
        , path
            [ Attributes.d "M236 39.5C236 50.2696 227.27 59 216.5 59C205.73 59 197 50.2696 197 39.5C197 28.7304 205.73 20 216.5 20C227.27 20 236 28.7304 236 39.5Z"
            , Attributes.fill "#3D3434"
            ]
            []
        , path
            [ Attributes.d "M236 216.5C236 227.27 227.27 236 216.5 236C205.73 236 197 227.27 197 216.5C197 205.73 205.73 197 216.5 197C227.27 197 236 205.73 236 216.5Z"
            , Attributes.fill "#3D3434"
            ]
            []
        , path
            [ Attributes.d "M59 128C59 139.598 50.2696 149 39.5 149C28.7304 149 20 139.598 20 128C20 116.402 28.7304 107 39.5 107C50.2696 107 59 116.402 59 128Z"
            , Attributes.fill "#3D3434"
            ]
            []
        ]


iconToggleCarDebug : Svg msg
iconToggleCarDebug =
    svg
        [ Attributes.width "256"
        , Attributes.height "256"
        , Attributes.viewBox "0 0 256 256"
        , Attributes.fill "none"
        ]
        [ path
            [ Attributes.d "M122.327 187.302H198.426H222C227.523 187.302 232 182.825 232 177.302V155.995V79.9651V58.6584C232 53.1356 227.523 48.6584 222 48.6584H198.426H126.803"
            , Attributes.stroke "#E68C4D"
            , Attributes.strokeWidth "5"
            ]
            []
        , Svg.circle
            [ Attributes.cx "126.465"
            , Attributes.cy "48.6584"
            , Attributes.r "19.6584"
            , Attributes.fill "#3D3434"
            ]
            []
        , path
            [ Attributes.d "M23 187.501C23 197.918 24.4592 209.441 25.6595 217.158C26.4838 222.457 30.8935 226.362 36.2514 226.597C42.2065 226.858 50.6267 227.095 60.0449 226.962C70.7311 226.811 77.4099 225.201 88.5231 225.201C102.374 225.201 125.085 226.761 139.868 226.962C148.433 227.078 156.113 226.867 161.703 226.622C167.459 226.37 172.519 222.856 174.387 217.406C176.938 209.966 180 198.704 180 187.501C180 176.297 176.938 165.034 174.387 157.594C172.518 152.145 167.459 148.631 161.704 148.379C156.114 148.134 148.433 147.922 139.868 148.038C125.085 148.239 102.374 149.8 88.5231 149.8C77.4099 149.8 70.7311 148.189 60.0449 148.038C50.6263 147.905 42.2057 148.142 36.2505 148.404C30.893 148.639 26.4839 152.544 25.6596 157.843C24.4593 165.559 23 177.083 23 187.501Z"
            , Attributes.fill "#318CE7"
            , Attributes.stroke "#1978D7"
            , Attributes.strokeWidth "5"
            ]
            []
        , path
            [ Attributes.d "M168.534 161.214C167.652 157.25 166.77 151.524 166.77 151.524H169.416L173.385 155.928L175.59 168.262C175.59 168.262 169.416 165.178 168.534 161.214Z"
            , Attributes.fill "#F9F9E9"
            , Attributes.stroke "#1978D7"
            ]
            []
        , path
            [ Attributes.d "M168.534 214.07C167.652 218.035 166.77 223.761 166.77 223.761H169.416L173.385 219.356L175.59 207.023C175.59 207.023 169.416 210.106 168.534 214.07Z"
            , Attributes.fill "#F9F9E9"
            , Attributes.stroke "#1978D7"
            ]
            []
        , path
            [ Attributes.d "M23 187.501C23 197.918 24.4592 209.441 25.6595 217.158C26.4838 222.457 30.8935 226.362 36.2514 226.597C42.2065 226.858 50.6267 227.095 60.0449 226.962C70.7311 226.811 77.4099 225.201 88.5231 225.201C102.374 225.201 125.085 226.761 139.868 226.962C148.433 227.078 156.113 226.867 161.703 226.622C167.459 226.37 172.519 222.856 174.387 217.406C176.938 209.966 180 198.704 180 187.501C180 176.297 176.938 165.034 174.387 157.594C172.518 152.145 167.459 148.631 161.704 148.379C156.114 148.134 148.433 147.922 139.868 148.038C125.085 148.239 102.374 149.8 88.5231 149.8C77.4099 149.8 70.7311 148.189 60.0449 148.038C50.6263 147.905 42.2057 148.142 36.2505 148.404C30.893 148.639 26.4839 152.544 25.6596 157.843C24.4593 165.559 23 177.083 23 187.501Z"
            , Attributes.stroke "#3D3434"
            , Attributes.strokeWidth "5"
            ]
            []
        , path
            [ Attributes.d "M102.417 216.464L117.335 218.112C120.478 218.459 123.363 216.304 123.847 213.156C124.794 207 126.314 195.919 126.314 188.121C126.314 180.333 124.798 168.855 123.851 162.437C123.37 159.18 120.333 156.983 117.099 157.468L102.258 159.692C100.235 159.995 98.7668 161.774 98.8477 163.817C99.0502 168.925 99.4575 180.19 99.4575 188.121C99.4575 195.998 99.0558 207.163 98.8519 212.319C98.7688 214.422 100.324 216.232 102.417 216.464Z"
            , Attributes.fill "#F9F9E9"
            , Attributes.stroke "#1978D7"
            , Attributes.strokeWidth "2"
            ]
            []
        , path
            [ Attributes.d "M72.8695 213.047L60.6363 213.273C57.8445 213.325 55.3671 211.441 54.7721 208.68C53.693 203.671 51.9888 194.722 51.9888 188.058C51.9888 181.41 53.6851 172.139 54.7639 166.904C55.3568 164.027 57.9787 162.09 60.8802 162.257L73.0307 162.957C75.2103 163.083 76.8871 164.929 76.8053 167.11C76.6254 171.907 76.2993 181.509 76.2993 188.058C76.2993 194.552 76.6199 204.048 76.8007 208.885C76.8845 211.125 75.1153 213.006 72.8695 213.047Z"
            , Attributes.fill "#F9F9E9"
            , Attributes.stroke "#1978D7"
            , Attributes.strokeWidth "2"
            ]
            []
        ]


iconSpawnCar : Svg msg
iconSpawnCar =
    svg
        [ Attributes.width "256"
        , Attributes.height "256"
        , Attributes.viewBox "0 0 256 256"
        , Attributes.fill "none"
        ]
        [ path
            [ Attributes.d "M24 178.876C24 194.259 26.3668 211.444 28.0262 221.602C28.8912 226.897 33.295 230.802 38.6536 231.069C46.6161 231.466 59.1396 231.901 73.3736 231.699C87.6161 231.496 96.5177 229.342 111.329 229.342C129.79 229.342 160.059 231.43 179.762 231.699C192.844 231.877 204.378 231.479 211.944 231.098C217.706 230.809 222.759 227.289 224.727 221.867C228.268 212.113 233.25 195.446 233.25 178.876C233.25 162.305 228.268 145.638 224.727 135.884C222.759 130.462 217.707 126.943 211.945 126.653C204.379 126.271 192.845 125.873 179.762 126.051C160.059 126.32 129.79 128.41 111.329 128.41C96.5177 128.41 87.6161 126.254 73.3736 126.051C59.1391 125.849 46.6152 126.284 38.6528 126.682C33.2946 126.95 28.8913 130.854 28.0263 136.149C26.3669 146.307 24 163.492 24 178.876Z"
            , Attributes.fill "#318CE7"
            , Attributes.stroke "#1978D7"
            , Attributes.strokeWidth "5"
            ]
            []
        , path
            [ Attributes.d "M217.968 143.689C216.792 138.382 215.617 130.717 215.617 130.717H219.143L224.433 136.613L227.372 153.122C227.372 153.122 219.143 148.995 217.968 143.689Z"
            , Attributes.fill "#F9F9E9"
            , Attributes.stroke "#1978D7"
            ]
            []
        , path
            [ Attributes.d "M217.968 214.443C216.792 219.749 215.617 227.414 215.617 227.414H219.143L224.433 221.518L227.372 205.009C227.372 205.009 219.143 209.136 217.968 214.443Z"
            , Attributes.fill "#F9F9E9"
            , Attributes.stroke "#1978D7"
            ]
            []
        , path
            [ Attributes.d "M24 178.876C24 194.259 26.3668 211.444 28.0262 221.602C28.8912 226.897 33.295 230.802 38.6536 231.069C46.6161 231.466 59.1396 231.901 73.3736 231.699C87.6161 231.496 96.5177 229.342 111.329 229.342C129.79 229.342 160.059 231.43 179.762 231.699C192.844 231.877 204.378 231.479 211.944 231.098C217.706 230.809 222.759 227.289 224.727 221.867C228.268 212.113 233.25 195.446 233.25 178.876C233.25 162.305 228.268 145.638 224.727 135.884C222.759 130.462 217.707 126.943 211.945 126.653C204.379 126.271 192.845 125.873 179.762 126.051C160.059 126.32 129.79 128.41 111.329 128.41C96.5177 128.41 87.6161 126.254 73.3736 126.051C59.1391 125.849 46.6152 126.284 38.6528 126.682C33.2946 126.95 28.8913 130.854 28.0263 136.149C26.3669 146.307 24 163.492 24 178.876Z"
            , Attributes.stroke "#3D3434"
            , Attributes.strokeWidth "5"
            ]
            []
        , path
            [ Attributes.d "M128.952 217.206L151.312 219.687C154.456 220.036 157.341 217.89 157.836 214.742C159.063 206.933 161.365 190.813 161.365 179.706C161.365 168.611 159.067 151.877 157.84 143.725C157.349 140.468 154.311 138.28 151.075 138.766L128.793 142.12C126.773 142.424 125.304 144.194 125.386 146.239C125.639 152.545 126.236 168.574 126.236 179.706C126.236 190.773 125.646 206.679 125.391 213.062C125.306 215.164 126.861 216.974 128.952 217.206Z"
            , Attributes.fill "#F9F9E9"
            , Attributes.stroke "#1978D7"
            , Attributes.strokeWidth "2"
            ]
            []
        , path
            [ Attributes.d "M66.9494 149.987L67.9279 150.193L66.9494 149.987C67.5546 147.114 70.1747 145.174 73.0796 145.342L91.6445 146.417C93.8209 146.543 95.5002 148.38 95.4171 150.564C95.1889 156.553 94.7045 170.362 94.7045 179.622C94.7045 188.821 95.1825 202.509 95.4125 208.561C95.4978 210.805 93.7243 212.673 91.4827 212.715L72.8348 213.061C70.0401 213.113 67.5655 211.228 66.9577 208.472C65.5546 202.109 62.9691 189.112 62.9691 179.622C62.9691 170.151 65.5455 156.649 66.9494 149.987Z"
            , Attributes.fill "#F9F9E9"
            , Attributes.stroke "#1978D7"
            , Attributes.strokeWidth "2"
            ]
            []
        , path
            [ Attributes.d "M207.5 28C207.5 24.4101 204.59 21.5 201 21.5C197.41 21.5 194.5 24.4101 194.5 28V48.5H174C170.41 48.5 167.5 51.4102 167.5 55C167.5 58.5899 170.41 61.5 174 61.5H194.5V82C194.5 85.5899 197.41 88.5 201 88.5C204.59 88.5 207.5 85.5899 207.5 82V61.5H228C231.59 61.5 234.5 58.5899 234.5 55C234.5 51.4102 231.59 48.5 228 48.5H207.5V28Z"
            , Attributes.fill "#E58C4D"
            , Attributes.stroke "#3D3434"
            , Attributes.strokeWidth "3"
            , Attributes.strokeLinecap "round"
            , Attributes.strokeLinejoin "round"
            ]
            []
        ]
