module PathfindingVisualization exposing (main)

import Browser
import Browser.Events as Events
import CubicSpline2d exposing (Nondegenerate)
import Data.Cars exposing (testCar)
import Data.Colors as Colors
import Direction2d
import Duration exposing (Duration)
import Length
import Model.Car as Car exposing (Car)
import Model.Geometry exposing (GlobalCoordinates, LMCubicSpline2d, LMDirection2d, LMPoint2d)
import Point2d
import Render
import Render.Conversion exposing (toPixelsValue)
import Render.Shape
import Splines
import Svg exposing (Svg)
import Svg.Attributes as Attributes


renderArea : Length.Length
renderArea =
    Length.meters 24


renderAreaWidthStr : String
renderAreaWidthStr =
    String.fromFloat (toPixelsValue renderArea)


renderAreaHeightStr : String
renderAreaHeightStr =
    String.fromFloat (toPixelsValue renderArea)


start : LMPoint2d
start =
    Point2d.fromMeters { x = 2, y = 2 }


end : LMPoint2d
end =
    Point2d.fromMeters { x = 20, y = 20 }


initialTangentDirection =
    Direction2d.positiveX


spline : LMCubicSpline2d
spline =
    Splines.curveSpline
        Splines.Natural
        start
        end
        initialTangentDirection


type alias Model =
    { spline : Maybe (Nondegenerate Length.Meters GlobalCoordinates)
    , parameter : Float
    , pointOnSpline : LMPoint2d
    , tangentDirection : LMDirection2d
    , car : Car
    }


type Msg
    = AnimationFrameReceived Duration


main : Program () Model Msg
main =
    let
        car =
            Car.new testCar
                |> Car.withPosition start
                |> Car.withOrientation (Direction2d.toAngle initialTangentDirection)
                |> Car.build 1

        initialModel =
            { spline = CubicSpline2d.nondegenerate spline |> Result.toMaybe
            , parameter = 0
            , pointOnSpline = start
            , tangentDirection = initialTangentDirection
            , car = car
            }
    in
    Browser.element
        { init = \_ -> ( initialModel, Cmd.none )
        , update = update
        , view = view
        , subscriptions = \_ -> Events.onAnimationFrameDelta (Duration.milliseconds >> AnimationFrameReceived)
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AnimationFrameReceived delta ->
            let
                ( nextPointOnSpline, nextTangentDirection ) =
                    case model.spline of
                        Just validSpline ->
                            CubicSpline2d.sample validSpline (min model.parameter 1.0)

                        _ ->
                            ( model.pointOnSpline, model.tangentDirection )

                car =
                    model.car

                nextCar =
                    { car
                        | position = nextPointOnSpline
                        , orientation = Direction2d.toAngle nextTangentDirection
                    }
            in
            ( { model
                | parameter = model.parameter + (Duration.inMilliseconds delta / 3000)
                , pointOnSpline = nextPointOnSpline
                , tangentDirection = nextTangentDirection
                , car = nextCar
              }
            , Cmd.none
            )


view : Model -> Svg Msg
view model =
    Svg.svg
        [ Attributes.width renderAreaWidthStr
        , Attributes.height renderAreaHeightStr
        , Attributes.viewBox <| "0 0 " ++ renderAreaWidthStr ++ " " ++ renderAreaHeightStr
        , Attributes.style <| "background-color: " ++ Colors.lightGreenCSS ++ ";"
        ]
        [ Render.Shape.cubicSpline
            Colors.gray6
            renderArea
            spline
        , Render.Shape.circle Colors.gray1 renderArea model.pointOnSpline (Length.meters 1)
        , Render.renderCar (Render.Conversion.toPixelsValue renderArea) model.car
        ]
