module PathfindingVisualization exposing
    ( Model
    , Msg
    , Path
    , SplineMeta
    , main
    )

import Array exposing (Array)
import Browser
import Browser.Events as Events
import Common exposing (GlobalCoordinates)
import CubicSpline2d exposing (ArcLengthParameterized, Nondegenerate)
import Data.Cars exposing (testCar)
import Data.Colors as Colors
import Data.Utility exposing (testSeed)
import Direction2d exposing (Direction2d)
import Duration exposing (Duration)
import Html exposing (Html)
import Length
import Lib.Collection exposing (initialId)
import Model.RenderCache as RenderCache exposing (RenderCache)
import Model.World as World exposing (World)
import Point2d exposing (Point2d)
import Quantity
import Render
import Render.Conversion exposing (defaultPixelsToMetersRatio, toPixelsValue)
import Render.Shape
import Round
import Simulation.Car as Car exposing (Car)
import Simulation.Splines as Splines
import Simulation.Steering as Steering
import Speed exposing (Speed)
import Svg
import Svg.Attributes as Attributes
import Svg.Lazy
import Vector2d


type alias Path =
    { splines : Array SplineMeta
    , currentSplineIdx : Int
    , currentSpline : Maybe SplineMeta
    , parameter : Length.Length
    , pointOnSpline : Point2d Length.Meters GlobalCoordinates
    , tangentDirection : Direction2d GlobalCoordinates
    }


type alias SplineMeta =
    { spline : ArcLengthParameterized Length.Meters GlobalCoordinates
    , length : Length.Length
    }


world : World
world =
    World.empty
        testSeed
        { horizontalCellsAmount = 4
        , verticalCellsAmount = 4
        }


renderCache : RenderCache
renderCache =
    RenderCache.new world


renderArea : Length.Length
renderArea =
    Length.meters 64


renderAreaWidthStr : String
renderAreaWidthStr =
    String.fromFloat (toPixelsValue defaultPixelsToMetersRatio renderArea)


renderAreaHeightStr : String
renderAreaHeightStr =
    String.fromFloat (toPixelsValue defaultPixelsToMetersRatio renderArea)


nodes : List ( Point2d Length.Meters GlobalCoordinates, Direction2d GlobalCoordinates )
nodes =
    [ ( Point2d.fromMeters { x = 30, y = 20 }, Direction2d.positiveY )
    , ( Point2d.fromMeters { x = 20, y = 35 }, Direction2d.negativeX )
    , ( Point2d.fromMeters { x = 10, y = 45 }, Direction2d.positiveY )
    , ( Point2d.fromMeters { x = 20, y = 55 }, Direction2d.positiveX )
    , ( Point2d.fromMeters { x = 30, y = 55 }, Direction2d.positiveX )
    , ( Point2d.fromMeters { x = 50, y = 40 }, Direction2d.negativeY )
    , ( Point2d.fromMeters { x = 50, y = 10 }, Direction2d.negativeY )
    ]


initialStart : Point2d Length.Meters GlobalCoordinates
initialStart =
    Point2d.fromMeters { x = 2, y = 2 }


initialTangentDirection : Direction2d GlobalCoordinates
initialTangentDirection =
    Direction2d.positiveX


initialPath : Path
initialPath =
    let
        splines =
            createPath
                initialStart
                initialTangentDirection
                nodes
                Array.empty
    in
    { splines = splines
    , currentSplineIdx = 0
    , currentSpline = Array.get 0 splines
    , parameter = Quantity.zero
    , pointOnSpline = initialStart
    , tangentDirection = initialTangentDirection
    }


createPath :
    Point2d Length.Meters GlobalCoordinates
    -> Direction2d GlobalCoordinates
    -> List ( Point2d Length.Meters GlobalCoordinates, Direction2d GlobalCoordinates )
    -> Array SplineMeta
    -> Array SplineMeta
createPath start startTangentDirection others acc =
    case others of
        ( end, endTangentDirection ) :: xs ->
            let
                spline =
                    if parallel start end then
                        Splines.straightSpline start end

                    else
                        Splines.curveSpline
                            start
                            end
                            startTangentDirection
                            0.8

                nextAcc =
                    case CubicSpline2d.nondegenerate spline of
                        Ok ndSpline ->
                            let
                                length =
                                    splineLengthALP ndSpline

                                splineProps =
                                    { spline = CubicSpline2d.arcLengthParameterized { maxError = Length.meters 0.1 } ndSpline
                                    , length = length
                                    }
                            in
                            Array.push splineProps acc

                        Err _ ->
                            acc
            in
            createPath
                end
                endTangentDirection
                xs
                nextAcc

        [] ->
            acc


parallel : Point2d Length.Meters GlobalCoordinates -> Point2d Length.Meters GlobalCoordinates -> Bool
parallel p1 p2 =
    let
        p1v =
            Point2d.toMeters p1

        p2v =
            Point2d.toMeters p2
    in
    p1v.x == p2v.x || p1v.y == p2v.y


splineLengthALP : Nondegenerate Length.Meters GlobalCoordinates -> Length.Length
splineLengthALP ndSpline =
    ndSpline
        |> CubicSpline2d.arcLengthParameterized { maxError = Length.meters 0.1 }
        |> CubicSpline2d.arcLength


type alias Model =
    { path : Path
    , car : Car
    }


initialCar : Car
initialCar =
    Car.new testCar
        |> Car.withPosition initialStart
        |> Car.withOrientation (Direction2d.toAngle initialTangentDirection)
        |> Car.withVelocity Steering.maxVelocity
        |> Car.build Nothing
        |> (\builderFn -> builderFn initialId)


initialModel : Model
initialModel =
    { path = initialPath
    , car = initialCar
    }


type Msg
    = AnimationFrameReceived Duration


main : Program () Model Msg
main =
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
                ( splineIdx, splineMeta ) =
                    if
                        model.path.parameter
                            |> Quantity.greaterThanOrEqualTo
                                (model.path.currentSpline
                                    |> Maybe.map .length
                                    |> Maybe.withDefault Quantity.zero
                                )
                    then
                        let
                            nextSplineIdx =
                                model.path.currentSplineIdx + 1
                        in
                        ( nextSplineIdx, Array.get nextSplineIdx model.path.splines )

                    else
                        ( model.path.currentSplineIdx, model.path.currentSpline )

                nextPath =
                    case splineMeta of
                        Just meta ->
                            updatePath model.path model.car.velocity delta splineIdx meta

                        Nothing ->
                            initialPath

                car =
                    model.car

                nextCar =
                    { car
                        | position = nextPath.pointOnSpline
                        , orientation = Direction2d.toAngle nextPath.tangentDirection
                    }
            in
            ( { path = nextPath
              , car = nextCar
              }
            , Cmd.none
            )


updatePath : Path -> Speed -> Duration -> Int -> SplineMeta -> Path
updatePath path velocity delta splineIdx splineMeta =
    let
        nextParameter =
            if splineIdx /= path.currentSplineIdx then
                Quantity.zero

            else
                updateParameter velocity delta path.parameter

        ( nextPointOnSpline, nextTangentDirection ) =
            CubicSpline2d.sampleAlong splineMeta.spline nextParameter
    in
    { splines = path.splines
    , parameter = nextParameter
    , currentSplineIdx = splineIdx
    , currentSpline = Just splineMeta
    , pointOnSpline = nextPointOnSpline
    , tangentDirection = nextTangentDirection
    }


updateParameter : Speed -> Duration -> Length.Length -> Length.Length
updateParameter velocity delta parameter =
    let
        deltaMeters =
            velocity |> Quantity.for delta
    in
    parameter |> Quantity.plus deltaMeters


view : Model -> Html Msg
view model =
    Html.div []
        [ Svg.svg
            [ Attributes.width renderAreaWidthStr
            , Attributes.height renderAreaHeightStr
            , Attributes.viewBox <| "0 0 " ++ renderAreaWidthStr ++ " " ++ renderAreaHeightStr
            , Attributes.style <| "background-color: " ++ Colors.lightGreenCSS ++ ";"
            ]
            [ Svg.Lazy.lazy renderPath model.path.splines
            , Render.Shape.circle
                renderCache
                Colors.gray1
                Nothing
                (Length.meters 1)
                model.path.pointOnSpline
            , Render.renderCarLazy renderCache model.car
            ]
        , Html.div []
            [ Html.pre [] [ Html.text ("Parameter " ++ Round.round 2 (Length.inMeters model.path.parameter)) ]
            , Html.pre []
                [ Html.text
                    (debugVector
                        (model.path.pointOnSpline
                            |> Point2d.toMeters
                            |> Vector2d.fromUnitless
                        )
                        "Point on path"
                    )
                ]
            , Html.pre []
                [ Html.text
                    (debugVector (model.path.tangentDirection |> Direction2d.toVector) "Tangent direction")
                ]
            ]
        ]


renderPath : Array SplineMeta -> Svg.Svg Msg
renderPath =
    Array.map
        (\splineMeta ->
            Render.Shape.cubicSplineDebug
                renderCache
                Colors.gray6
                (CubicSpline2d.fromArcLengthParameterized splineMeta.spline)
        )
        >> Array.toList
        >> Svg.g []


debugVector : Vector2d.Vector2d Quantity.Unitless GlobalCoordinates -> String -> String
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
