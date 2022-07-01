module PathfindingVisualization exposing (main)

import Array exposing (Array)
import Browser
import Browser.Events as Events
import CubicSpline2d exposing (Nondegenerate)
import Data.Cars exposing (testCar)
import Data.Colors as Colors
import Direction2d
import Duration exposing (Duration)
import Html exposing (Html)
import Length
import Model.Car as Car exposing (Car)
import Model.Geometry exposing (GlobalCoordinates, LMCubicSpline2d, LMDirection2d, LMPoint2d)
import Point2d
import Render
import Render.Conversion exposing (toPixelsValue)
import Render.Shape
import Round
import Splines
import Svg
import Svg.Attributes as Attributes
import Svg.Lazy
import Vector2d


renderArea : Length.Length
renderArea =
    Length.meters 64


renderAreaWidthStr : String
renderAreaWidthStr =
    String.fromFloat (toPixelsValue renderArea)


renderAreaHeightStr : String
renderAreaHeightStr =
    String.fromFloat (toPixelsValue renderArea)


initialStart : LMPoint2d
initialStart =
    Point2d.fromMeters { x = 2, y = 2 }


initialTangentDirection =
    Direction2d.positiveX


nodes : List ( LMPoint2d, LMDirection2d )
nodes =
    [ ( Point2d.fromMeters { x = 30, y = 20 }, Direction2d.positiveY )
    , ( Point2d.fromMeters { x = 20, y = 35 }, Direction2d.negativeX )
    , ( Point2d.fromMeters { x = 10, y = 45 }, Direction2d.positiveY )
    , ( Point2d.fromMeters { x = 20, y = 55 }, Direction2d.positiveX )
    , ( Point2d.fromMeters { x = 30, y = 55 }, Direction2d.positiveX )
    , ( Point2d.fromMeters { x = 50, y = 40 }, Direction2d.negativeY )
    , ( Point2d.fromMeters { x = 50, y = 10 }, Direction2d.negativeY )
    ]


path : Array (Nondegenerate Length.Meters GlobalCoordinates)
path =
    pathHelper
        initialStart
        initialTangentDirection
        nodes
        Array.empty


pathHelper :
    LMPoint2d
    -> LMDirection2d
    -> List ( LMPoint2d, LMDirection2d )
    -> Array (Nondegenerate Length.Meters GlobalCoordinates)
    -> Array (Nondegenerate Length.Meters GlobalCoordinates)
pathHelper start startTangentDirection others acc =
    case others of
        ( end, endTangentDirection ) :: xs ->
            let
                spline =
                    if parallel start end then
                        Splines.straightSpline start end

                    else
                        Splines.curveSpline
                            Splines.Natural
                            start
                            end
                            startTangentDirection

                nextAcc =
                    case CubicSpline2d.nondegenerate spline of
                        Ok ngSpline ->
                            Array.push ngSpline acc

                        Err _ ->
                            acc
            in
            pathHelper
                end
                endTangentDirection
                xs
                nextAcc

        [] ->
            acc


parallel : LMPoint2d -> LMPoint2d -> Bool
parallel p1 p2 =
    let
        p1v =
            Point2d.toMeters p1

        p2v =
            Point2d.toMeters p2
    in
    p1v.x == p2v.x || p1v.y == p2v.y


type alias Model =
    { path : Array (Nondegenerate Length.Meters GlobalCoordinates)
    , currentNodeIdx : Int
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
                |> Car.withPosition initialStart
                |> Car.withOrientation (Direction2d.toAngle initialTangentDirection)
                |> Car.build 1

        initialModel =
            { path = path
            , currentNodeIdx = 0
            , parameter = 0
            , pointOnSpline = initialStart
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
                ( nodeIdx, nextParameter ) =
                    if model.parameter >= 1.0 then
                        -- Start from the beginning of next spline
                        ( model.currentNodeIdx + 1
                        , 0
                        )

                    else
                        -- Sample current spline
                        ( model.currentNodeIdx
                        , model.parameter + (Duration.inMilliseconds delta / 4000)
                        )

                ( nextPointOnSpline, nextTangentDirection ) =
                    case Array.get nodeIdx model.path of
                        Just validSpline ->
                            sample nextParameter validSpline

                        _ ->
                            ( initialStart, initialTangentDirection )

                car =
                    model.car

                nextCar =
                    { car
                        | position = nextPointOnSpline
                        , orientation = Direction2d.toAngle nextTangentDirection
                    }
            in
            ( { model
                | parameter = nextParameter
                , currentNodeIdx = nodeIdx
                , pointOnSpline = nextPointOnSpline
                , tangentDirection = nextTangentDirection
                , car = nextCar
              }
            , Cmd.none
            )


sample : Float -> Nondegenerate Length.Meters GlobalCoordinates -> ( LMPoint2d, LMDirection2d )
sample parameter ngSpline =
    let
        spline =
            CubicSpline2d.fromNondegenerate ngSpline

        shouldSampleLinear =
            (CubicSpline2d.startDerivative spline == Vector2d.zero)
                && (CubicSpline2d.endDerivative spline == Vector2d.zero)
    in
    if shouldSampleLinear then
        -- A straight cubic bezier curve (zero start and end derivative) should be sampled linearly
        -- to avoid weird behavior near the start and end points
        sampleLinear parameter spline

    else
        CubicSpline2d.sample ngSpline parameter


sampleLinear : Float -> LMCubicSpline2d -> ( LMPoint2d, LMDirection2d )
sampleLinear parameter spline =
    let
        cp1 =
            CubicSpline2d.firstControlPoint spline

        cp4 =
            CubicSpline2d.fourthControlPoint spline
    in
    ( Point2d.interpolateFrom
        cp1
        cp4
        parameter
    , Direction2d.from cp1 cp4 |> Maybe.withDefault Direction2d.positiveX
    )


view : Model -> Html Msg
view model =
    Html.div []
        [ Svg.svg
            [ Attributes.width renderAreaWidthStr
            , Attributes.height renderAreaHeightStr
            , Attributes.viewBox <| "0 0 " ++ renderAreaWidthStr ++ " " ++ renderAreaHeightStr
            , Attributes.style <| "background-color: " ++ Colors.lightGreenCSS ++ ";"
            ]
            [ Svg.Lazy.lazy renderPath model.path
            , Render.Shape.circle Colors.gray1 renderArea model.pointOnSpline (Length.meters 1)
            , Render.renderCar (Render.Conversion.toPixelsValue renderArea) model.car
            ]
        , Html.div []
            [ Html.pre [] [ Html.text ("Parameter " ++ Round.round 2 model.parameter) ]
            , Html.pre []
                [ Html.text
                    (debugVector
                        (model.pointOnSpline
                            |> Point2d.toMeters
                            |> Vector2d.fromUnitless
                        )
                        "Point on path"
                    )
                ]
            , Html.pre []
                [ Html.text
                    (debugVector (model.tangentDirection |> Direction2d.toVector) "Tangent direction")
                ]
            ]
        ]


renderPath : Array (Nondegenerate Length.Meters GlobalCoordinates) -> Svg.Svg Msg
renderPath =
    Array.map
        (\ndSpline ->
            Render.Shape.cubicSpline
                Colors.gray6
                renderArea
                (CubicSpline2d.fromNondegenerate ndSpline)
        )
        >> Array.toList
        >> Svg.g []


debugVector vector label =
    let
        { x, y } =
            Vector2d.toUnitless vector
    in
    String.join " "
        [ label
        , "["
        , Round.round 2 x
        , ","
        , Round.round 2 y
        , "]"
        ]
