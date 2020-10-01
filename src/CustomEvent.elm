module CustomEvent exposing (onRightClick)

-- from https://github.com/dc25/minesweeperElm/blob/4eeddd03ed29369c4ba584756a9cf9ed2f67e192/src/RightClick.elm

import Html as HTML
import Html.Events as HEV
import Json.Decode as Json


onRightClick : msg -> HTML.Attribute msg
onRightClick msg =
    HEV.custom "contextmenu"
        (Json.succeed
            { message = msg
            , stopPropagation = True
            , preventDefault = True
            }
        )
