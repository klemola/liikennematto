module Main exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrameDelta)
import Collage exposing (..)
import Collage.Layout exposing (..)
import Collage.Render exposing (svg)
import Color exposing (..)
import Dict exposing (Dict)
import Dict.Extra as Extra
import Html exposing (Html)


main : Program () Model Msg
main =
    Browser.element { init = init, view = view, update = update, subscriptions = subs }


subs : Model -> Sub Msg
subs _ =
    onAnimationFrameDelta ((\dt -> dt / 1000) >> Tick)



-- Constants


boardSize =
    8


blockSize =
    64


groundColor =
    lightGreen


roadColor =
    darkGray



-- TODO: Randomize roads


roads : List Point
roads =
    [ ( 0, 0 )
    , ( 1, 0 )
    , ( 2, 0 )
    , ( 3, 0 )
    , ( 4, 0 )
    , ( 4, 1 )
    , ( 4, 2 )
    , ( 4, 3 )
    , ( 4, 4 )
    , ( 5, 4 )
    , ( 6, 4 )
    , ( 7, 4 )
    ]



-- Model


type Direction
    = Up
    | Right
    | Down
    | Left


type alias Point =
    ( Int, Int )


type alias Car =
    { coords : Point
    , moving : Bool
    , direction : Direction
    , color : Color
    }


type alias Cars =
    Dict Int Car


type alias Delta =
    Float


type alias Model =
    { cars : Cars
    , time : Float
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { cars = initialCars, time = 0 }, Cmd.none )


initialCars : Cars
initialCars =
    Dict.fromList
        [ makeCarEntry 1 ( 0, 0 ) Right lightBlue
        ]


makeCarEntry : Int -> Point -> Direction -> Color -> ( Int, Car )
makeCarEntry index pt dir c =
    ( index, { moving = True, direction = dir, coords = pt, color = c } )



-- Update


type Msg
    = Tick Delta


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick dt ->
            let
                newTime =
                    model.time + dt

                secsPassed =
                    round model.time

                newSecs =
                    round newTime

                -- Temporarily limit ticks to ~one per sec
                shouldUpdate =
                    secsPassed /= newSecs
            in
            ( { model
                | cars =
                    if shouldUpdate then
                        tick dt model.cars

                    else
                        model.cars
                , time = newTime
              }
            , Cmd.none
            )


tick : Float -> Cars -> Cars
tick dt cars =
    Dict.map (\_ c -> updateCar c) cars


updateCar : Car -> Car
updateCar car =
    let
        updatedCoords dir coords =
            if isRoad coords then
                ( coords, dir )

            else
                ( car.coords, turn car )

        ( uCoords, uDir ) =
            updatedCoords car.direction (nextCoords car.direction car.coords)
    in
    if car.moving then
        { car | coords = uCoords, direction = uDir }

    else
        car


nextCoords : Direction -> Point -> Point
nextCoords dir ( x, y ) =
    case dir of
        Up ->
            ( x, y - 1 )

        Right ->
            ( x + 1, y )

        Down ->
            ( x, y + 1 )

        Left ->
            ( x - 1, y )


turn : Car -> Direction
turn car =
    case car.direction of
        Up ->
            Right

        Right ->
            Down

        Down ->
            Left

        Left ->
            Up


isRoad : Point -> Bool
isRoad point =
    List.member point roads



-- View


view : Model -> Html Msg
view model =
    boardElement model |> svg


{-| A 2D board of boardSize x boardSize tiles.
Contains grass, roads and cars.
-}
boardElement : Model -> Collage msg
boardElement model =
    let
        rg =
            List.range 0 (boardSize - 1)

        makeTile x y =
            case Extra.find (\_ car -> car.coords == ( x, y )) model.cars of
                Just ( _, car ) ->
                    stack [ carElement car, tileElement <| tileColor ( x, y ) ]

                Nothing ->
                    tileElement <| tileColor ( x, y )

        col x =
            vertical <|
                List.map
                    (makeTile x)
                    rg
    in
    horizontal <|
        List.map col rg


tileElement : Color -> Collage msg
tileElement c =
    rectangle blockSize blockSize
        |> styled ( uniform c, border )


carElement : Car -> Collage msg
carElement car =
    let
        rotationDegrees =
            case car.direction of
                Up ->
                    0

                Right ->
                    270

                Down ->
                    180

                Left ->
                    90

        tri =
            triangle (blockSize / 2)
                |> styled ( uniform car.color, border )

        -- Denotes direction
        ln =
            path [ ( 0, 0 - (blockSize / 2) ), ( 0, blockSize / 2 ) ]
                |> traced (solid thin (uniform black))
    in
    stack [ ln, tri ]
        |> rotate (degrees rotationDegrees)


border : LineStyle
border =
    solid thin <| uniform black


tileColor : Point -> Color
tileColor point =
    if isRoad point then
        roadColor

    else
        groundColor
