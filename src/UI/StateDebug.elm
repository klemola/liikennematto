module UI.StateDebug exposing
    ( devMenu
    , wfcContext
    , wfcCurrentCell
    , wfcStateDescription
    )

import Data.Colors
import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html
import Json.Encode as JE
import Length
import Lib.Collection as Collection
import Model.Liikennematto exposing (Liikennematto)
import Model.World exposing (World, formatEvents)
import Point2d exposing (Point2d)
import Quantity
import Render.Conversion
import Round
import Simulation.Car as Car exposing (Car)
import Speed exposing (Speed)
import Tilemap.Cell as Cell
import Tilemap.DrivenWFC exposing (DrivenWFC(..), drivenWfcDebug)
import Tilemap.WFC as WFC
import Time
import UI.Button exposing (textButton)
import UI.Core
    exposing
        ( borderSize
        , uiColorText
        , whitespaceRegular
        , whitespaceTight
        )
import UI.Model exposing (DevView(..))


debugElementLength : Element.Length
debugElementLength =
    Element.fill |> Element.maximum 420


borderRadiusPanel : Int
borderRadiusPanel =
    15


textSize : Int
textSize =
    14


textSizeMini : Int
textSizeMini =
    12


bgColor : Element.Color
bgColor =
    Element.rgba255 61 52 52 0.3


cardBgColor : Element.Color
cardBgColor =
    Element.rgb 118 101 101


devMenu : (DevView -> msg) -> DevView -> Liikennematto -> Element msg
devMenu onSelectView selectedView model =
    Element.column
        [ Element.alignLeft
        , Element.moveRight 10
        , Element.moveDown 10
        , Element.padding whitespaceRegular
        , Element.spacing whitespaceTight
        , Element.width (Element.px 600)
        , Background.color bgColor
        , Border.rounded borderRadiusPanel
        ]
        [ Element.row
            [ Element.spacing whitespaceTight
            , Font.size textSize
            ]
            [ textButton
                { onPress = onSelectView EventQueueList
                , selected = selectedView == EventQueueList
                , disabled = False
                }
                "Events"
            , textButton
                { onPress = onSelectView WFCOverview
                , selected = selectedView == WFCOverview
                , disabled = False
                }
                "WFC"
            , textButton
                { onPress = onSelectView CarsList
                , selected = selectedView == CarsList
                , disabled = False
                }
                "Cars"
            , textButton
                { onPress = onSelectView SavegameOutput
                , selected = selectedView == SavegameOutput
                , disabled = False
                }
                "Savegames"
            ]
        , case selectedView of
            EventQueueList ->
                eventQueueList model.time model.world

            WFCOverview ->
                wfcOutput model.debug.wfcLog model.time model.wfc

            CarsList ->
                carsList model

            SavegameOutput ->
                savegameOutput model
        ]


wfcOutput : List String -> Time.Posix -> DrivenWFC -> Element msg
wfcOutput wfcLog currentTime drivenWfc =
    Element.column
        [ Element.spacing whitespaceTight
        , Element.padding whitespaceRegular
        , Element.width Element.fill
        , Element.height (Element.px 420)
        , Font.family [ Font.monospace ]
        , Font.size textSizeMini
        , Background.color cardBgColor
        , Border.rounded borderRadiusPanel
        ]
        (case drivenWfc of
            WFCActive wfcModel ->
                [ wfcStateDescription wfcModel
                , wfcContext wfcModel
                ]

            _ ->
                [ Element.el []
                    (Element.text (drivenWfcDebug currentTime drivenWfc))
                , Element.column
                    [ Element.scrollbars
                    , Element.height debugElementLength
                    , Element.width Element.fill
                    , Font.family [ Font.monospace ]
                    ]
                    (List.map (\row -> Element.text row) wfcLog)
                ]
        )


carsList : Liikennematto -> Element msg
carsList model =
    if Collection.size model.world.cars == 0 then
        Element.none

    else
        Element.column
            [ Element.spacing whitespaceTight
            , Element.width Element.fill
            ]
            (List.map
                carStateCard
                (Collection.values model.world.cars)
            )


eventQueueList : Time.Posix -> World -> Element msg
eventQueueList time world =
    Element.column
        [ Element.spacing whitespaceTight
        , Element.width Element.fill
        ]
        (List.map eventCard (formatEvents time world))


savegameOutput : Liikennematto -> Element msg
savegameOutput model =
    Element.el
        [ Element.padding whitespaceRegular
        , Element.height (Element.px 600)
        , Element.scrollbarY
        ]
        (Element.html
            (Html.pre []
                [ Html.text
                    (case model.savegame of
                        Just sg ->
                            JE.encode 2 sg

                        Nothing ->
                            "<none>"
                    )
                ]
            )
        )



--
-- Card helpers
--


cardAttributes : Element.Length -> List (Element.Attribute msg)
cardAttributes height =
    [ Element.width Element.fill
    , Element.height height
    , Element.padding whitespaceTight
    , Element.spacing UI.Core.whitespaceTight
    , Element.clipX
    , Font.color uiColorText
    , Font.size 13
    , Background.color cardBgColor
    , Border.solid
    , Border.rounded 10
    , Border.width borderSize
    , Border.color cardBgColor
    ]


carStateCard : Car -> Element msg
carStateCard car =
    Element.row
        (cardAttributes (Element.px 64))
        [ Element.text ("# " ++ Collection.idToString car.id)
        , Element.column [ Element.spacing whitespaceTight ]
            [ Element.text (pointToString car.position)
            , Element.text (speedToString car.velocity)
            , Element.text (Car.statusDescription car)
            ]
        ]


eventCard : ( String, String, String ) -> Element msg
eventCard queueEvent =
    let
        ( kind, triggerAt, retries ) =
            queueEvent
    in
    Element.el
        (cardAttributes (Element.px 64))
        (Element.column
            [ Element.spacing UI.Core.whitespaceTight ]
            [ Element.text kind
            , Element.text triggerAt
            , Element.text retries
            ]
        )


wfcStateDescription : WFC.Model -> Element msg
wfcStateDescription wfcModel =
    Element.paragraph
        []
        [ Element.text (WFC.stateDebug wfcModel) ]


wfcContext : WFC.Model -> Element msg
wfcContext wfcModel =
    let
        { position, openSteps, previousSteps, backtrackCount } =
            WFC.contextDebug wfcModel
    in
    Element.column
        [ Element.spacing 8
        , Element.width Element.fill
        , Font.family [ Font.monospace ]
        , Font.size 14
        ]
        [ Element.column
            []
            (position |> List.map (\step -> Element.el [] (Element.text step)))
        , Element.column
            []
            (openSteps |> List.map (\step -> Element.el [] (Element.text step)))
        , Element.el
            []
            (Element.text ("Backtrack count: " ++ String.fromInt backtrackCount))
        , Element.column
            [ Element.height (Element.px 420)
            , Element.width Element.fill
            , Element.scrollbars
            , Element.paddingXY 0 8
            , Element.spacing 4
            ]
            (previousSteps |> List.map (\step -> Element.el [] (Element.text step)))
        ]


wfcCurrentCell : WFC.Model -> Element msg
wfcCurrentCell wfcModel =
    case WFC.toCurrentCell wfcModel of
        Just cell ->
            let
                tileSizePixels =
                    Render.Conversion.toPixelsValue Render.Conversion.defaultPixelsToMetersRatio Cell.size

                ( cellX, cellY ) =
                    Cell.coordinates cell

                cellSize =
                    Element.px (floor tileSizePixels)

                cursorColor =
                    case WFC.currentState wfcModel of
                        WFC.Failed _ ->
                            Data.Colors.red

                        _ ->
                            Data.Colors.gray7
            in
            Element.el
                [ Element.width cellSize
                , Element.height cellSize
                , Element.moveRight (toFloat (cellX - 1) * tileSizePixels)
                , Element.moveDown (toFloat (cellY - 1) * tileSizePixels)
                , Border.width 2
                , Border.rounded 4
                , Border.solid
                , Border.color
                    (Data.Colors.uiCompat cursorColor)
                ]
                Element.none

        Nothing ->
            Element.none



--
-- String conversion helpers
--


speedToString : Speed -> String
speedToString speed =
    let
        speedValue =
            speed
                |> Quantity.unwrap
                |> Round.round 2
    in
    "Speed: " ++ speedValue ++ " m/s"


pointToString : Point2d Length.Meters a -> String
pointToString point =
    let
        { x, y } =
            point
                |> Point2d.at Render.Conversion.defaultPixelsToMetersRatio
                |> Point2d.toPixels

        format n =
            n
                |> truncate
                |> String.fromInt
                |> String.padLeft 2 ' '
    in
    String.join
        " "
        [ "x:"
        , format x
        , "y:"
        , format y
        ]
