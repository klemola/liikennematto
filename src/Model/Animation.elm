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
    , delay : Duration
    , name : AnimationName
    , direction : Maybe OrthogonalDirection
    }


type AnimationName
    = Appear
    | Disappear
    | Grow


toStyleString : Animation -> String
toStyleString animation =
    let
        ( name, initialOpacity ) =
            case ( animation.name, animation.direction ) of
                ( Appear, Just _ ) ->
                    ( "appear-from-direction", "1.0" )

                ( Appear, Nothing ) ->
                    ( "appear-directionless", "1.0" )

                ( Disappear, _ ) ->
                    ( "disappear", "1.0" )

                ( Grow, _ ) ->
                    ( "grow", "0" )

        duration =
            (animation.duration
                |> Duration.inMilliseconds
                |> String.fromFloat
            )
                ++ "ms"

        delay =
            (animation.delay
                |> Duration.inMilliseconds
                |> String.fromFloat
            )
                ++ "ms"
    in
    String.join "\n"
        [ "animation-fill-mode: forwards;"
        , "transform-box: fill-box;"
        , "animation-delay:" ++ delay ++ ";"
        , "animation-duration:" ++ duration ++ ";"
        , "animation-name:" ++ name ++ ";"
        , "opacity:" ++ initialOpacity ++ ";"
        ]


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

@keyframes grow {
    0%   { scale: 45%;  opacity: 0.2; }
    50%  {              opacity: 0.9; }
    66%  { scale: 110%; opacity: 1; }
    100% { scale: 100%; opacity: 1; }
}
    """
