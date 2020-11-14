module UI exposing (Model, Msg(..), initialModel, recalculateDimensions, update, view)

import Car exposing (Car)
import Config exposing (borderRadius, borderSize, colors, whitespace)
import Dict
import Direction exposing (Orientation(..))
import Editor
import Element
    exposing
        ( Element
        , alignTop
        , centerX
        , centerY
        , clipX
        , column
        , el
        , fill
        , fillPortion
        , height
        , image
        , inFront
        , newTabLink
        , padding
        , paragraph
        , px
        , row
        , spacing
        , text
        , width
        )
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Graphics
import Html exposing (Html)
import Position
import Render
import SharedState
    exposing
        ( SharedState
        , SimulationSpeed(..)
        , SimulationState(..)
        )
import Task


type Msg
    = SetSimulationState SimulationState
    | RecalculateDimensions ( Int, Int )
    | EditorMsg Editor.Msg


type alias Model =
    { editor : Editor.Model
    , dimensions : Dimensions
    }


type alias Dimensions =
    { toolbar : Int
    , menu : Int
    , menuButton : Int
    , text : Int
    }


initialModel : Model
initialModel =
    { editor = Editor.initialModel
    , dimensions = maxDimensions
    }


update : SharedState -> Msg -> Model -> ( Model, SharedState, Cmd Msg )
update sharedState msg model =
    case msg of
        SetSimulationState state ->
            ( model
            , sharedState
                |> SharedState.setSimulationState state
            , Cmd.none
            )

        RecalculateDimensions ( screenWidth, screenHeight ) ->
            let
                dimensions =
                    nextDimensions model.dimensions ( toFloat screenWidth, toFloat screenHeight )
            in
            ( { model
                | dimensions = dimensions
              }
            , sharedState
            , Cmd.none
            )

        EditorMsg editorMsg ->
            let
                ( editor, sharedStateUpdate, cmd ) =
                    -- TODO: editor should be at same hierarchy level as UI
                    Editor.update sharedState editorMsg model.editor
            in
            ( { model | editor = editor }
            , sharedStateUpdate
            , Cmd.map EditorMsg cmd
            )


recalculateDimensions : ( Int, Int ) -> Cmd Msg
recalculateDimensions screenSize =
    Task.succeed screenSize
        |> Task.perform RecalculateDimensions


maxDimensions : Dimensions
maxDimensions =
    { toolbar = 71
    , menu = 200
    , menuButton = 18
    , text = 14
    }


nextDimensions : Dimensions -> ( Float, Float ) -> Dimensions
nextDimensions dimensions ( screenWidth, screenHeight ) =
    -- dimensions are calculated to make the board and the UI fit the screen
    -- landscape is the only supported orientation
    -- implicit square board
    let
        ( paddingX, paddingY ) =
            ( 60, 40 )

        initialSpace =
            screenWidth - paddingX

        availableUISpace =
            initialSpace * 0.4

        toolbarButtonSize =
            min maxDimensions.toolbar (availableUISpace * 0.15 |> floor)

        toolbarSize =
            min maxDimensions.toolbar (toolbarButtonSize + 21)

        menuSize =
            min maxDimensions.menu (floorToEven availableUISpace - toolbarSize)
    in
    { dimensions
        | toolbar = toolbarSize
        , menu = menuSize
    }


floorToEven : Float -> Int
floorToEven num =
    let
        floored =
            truncate num

        isEven =
            modBy 2 floored == 0
    in
    if isEven then
        floored

    else
        max (floored - 1) 0


view : SharedState -> Model -> Html Msg
view sharedState model =
    let
        simulation =
            Render.view sharedState
                |> Element.html

        editor =
            Editor.overlay sharedState model.editor
                |> Element.map EditorMsg

        toolbar =
            Editor.toolbar model.editor model.dimensions.toolbar
                |> Element.map EditorMsg

        simulationBorderColor =
            case sharedState.simulationState of
                Paused ->
                    colors.selected

                _ ->
                    colors.heavyBorder
    in
    Element.layout
        [ Background.color colors.mainBackground
        , width fill
        , height fill
        ]
        (el
            [ centerX
            , centerY
            , padding whitespace.regular
            ]
            (row
                [ spacing whitespace.regular
                ]
                [ toolbar
                , el
                    [ inFront editor
                    , alignTop
                    , Border.solid
                    , Border.width borderSize.heavy
                    , Border.rounded borderRadius.heavy
                    , Border.color simulationBorderColor
                    , Background.color colors.terrain
                    ]
                    simulation
                , menu sharedState model
                ]
            )
        )


menu : SharedState -> Model -> Element Msg
menu sharedState model =
    column
        [ Font.family [ Font.monospace ]
        , Font.color colors.text
        , Font.size model.dimensions.text
        , alignTop
        , width (px model.dimensions.menu)
        , padding whitespace.tight
        , spacing whitespace.regular
        , Background.color colors.toolbarBackground
        , Border.rounded borderRadius.heavy
        , Border.solid
        , Border.widthEach
            { top = borderSize.heavy
            , bottom = borderSize.heavy
            , left = borderSize.light
            , right = borderSize.light
            }
        , Border.color colors.heavyBorder
        ]
        [ simulationControl sharedState model
        , debug sharedState model
        , projectInfo
        ]


simulationControl : SharedState -> Model -> Element Msg
simulationControl { simulationState } { dimensions } =
    let
        isSelected speed =
            case simulationState of
                Simulation currentSpeed ->
                    speed == currentSpeed

                Paused ->
                    False
    in
    row
        [ width fill
        , spacing whitespace.tight
        ]
        [ controlButton dimensions.menuButton "⏸️" (SetSimulationState Paused) (simulationState == Paused)
        , controlButton dimensions.menuButton "🐌" (SetSimulationState (Simulation Slow)) (isSelected Slow)
        , controlButton dimensions.menuButton "🐇" (SetSimulationState (Simulation Medium)) (isSelected Medium)
        , controlButton dimensions.menuButton "🐆" (SetSimulationState (Simulation Fast)) (isSelected Fast)
        ]


controlButton : Int -> String -> Msg -> Bool -> Element Msg
controlButton fontSize label msg selected =
    Input.button
        [ Background.color colors.buttonBackground
        , width (fillPortion 1)
        , Font.size fontSize
        , padding whitespace.tight
        , Border.width borderSize.light
        , Border.rounded borderRadius.light
        , Border.solid
        , Border.color
            (if selected then
                colors.selected

             else
                colors.lightBorder
            )
        ]
        { onPress = Just msg
        , label = text label
        }


debug : SharedState -> Model -> Element Msg
debug sharedState { dimensions } =
    column [ spacing whitespace.tight, width fill ]
        (Dict.values sharedState.cars
            |> List.map (carStateView dimensions.text)
        )


carStateView : Int -> Car -> Element msg
carStateView fontSize car =
    let
        showCarKind =
            image [ width (px fontSize) ]
                { description = ""
                , src = "assets/" ++ Graphics.carAsset car
                }
    in
    row
        [ width fill
        , padding whitespace.tight
        , spacing whitespace.regular
        , clipX
        , Font.color colors.textInverse
        , Font.size 13
        , Background.color colors.listItemBackground
        , Border.solid
        , Border.rounded borderRadius.light
        , Border.width borderSize.light
        , Border.color colors.listItemBackground
        ]
        [ showCarKind
        , column [ spacing whitespace.tight ]
            [ text (Position.toString car.position)
            , text (Car.statusDescription car.status)
            ]
        ]


projectInfo : Element Msg
projectInfo =
    let
        link url label =
            newTabLink
                [ Font.color colors.link
                ]
                { url = url
                , label = text label
                }
    in
    column
        [ Font.family
            [ Font.typeface "Helvetica"
            , Font.typeface "sans-serif"
            ]
        , Background.color colors.textInverse
        , Border.rounded borderRadius.light
        , width fill
        , padding whitespace.regular
        , spacing whitespace.regular
        ]
        [ el
            [ Font.size 16
            , Font.bold
            ]
            (text "Liikennematto")
        , paragraph [] [ text "Prototype village builder game with a tiny scale. Inspired by traffic mats that children play with." ]
        , row
            [ spacing whitespace.tight
            , centerX
            ]
            [ link "https://github.com/klemola/liikennematto" "GitHub"
            , text "｜"
            , link "https://matiasklemola.com/liikennematto-dev-blog-one" "Blog"
            , text "｜"
            , link "https://twitter.com/MatiasKlemola" "Twitter"
            ]
        ]
