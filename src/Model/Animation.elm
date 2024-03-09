module Model.Animation exposing
    ( Animation
    , AnimationName(..)
    , keyframes
    , toStyleString
    )

import Duration exposing (Duration)
import Lib.OrthogonalDirection exposing (OrthogonalDirection)


type alias Animation =
    { duration : Duration
    , name : AnimationName
    , direction : Maybe OrthogonalDirection
    }


type AnimationName
    = Appear
    | Disappear


toStyleString : Animation -> String
toStyleString animation =
    let
        name =
            keyframeName animation

        duration =
            (animation.duration
                |> Duration.inMilliseconds
                |> String.fromFloat
            )
                ++ "ms"
    in
    String.join " "
        [ "animation:"
        , duration
        , name
        , "forwards"
        , ";"
        ]


keyframeName : Animation -> String
keyframeName animation =
    case ( animation.name, animation.direction ) of
        ( Appear, Just _ ) ->
            "appear-from-direction"

        ( Appear, Nothing ) ->
            "appear-directionless"

        ( Disappear, _ ) ->
            "disappear"


keyframes : String
keyframes =
    """
    @keyframes appear-from-direction {
        0%   { transform: translate(var(--xOffset), var(--yOffset)); }
        50%  { transform: translate(0,            , 0             ); }
        51%  { transform: scale(    1             , 1             ); }
        80%  { transform: scale(    var(--scaleX) , var(--scaleY) ); }
        100% { transform: scale(    1             , 1             ); }
    }

    @keyframes appear-directionless {
        0%   { transform: scale(1    , 1   ) rotate( 0deg  ); }
        15%  { transform: scale(1.05 , 1   ) rotate( 1.5deg); }
        50%  { transform: scale(1.1  , 1.05) rotate( 2deg  ); }
        85%  { transform: scale(1    , 1.05) rotate(-1.5deg); }
        100% { transform: scale(1    , 1   ) rotate( 0deg  ); }
    }

    @keyframes disappear {
        0%   { transform: scale(1  ); opacity: 1.0; }
        100% { transform: scale(1.5); opacity: 0.0; }
    }
    """
