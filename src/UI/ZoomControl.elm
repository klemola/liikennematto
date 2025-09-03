module UI.ZoomControl exposing (Model, Msg, initialModel, update, view)

import Element exposing (Element)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input
import UI.Core
    exposing
        ( borderSize
        , colorBorder
        , colorMenuBackground
        , colorZoomStepGuide
        , colorZoomThumbBackground
        , colorZoomTrackBackground
        , whitespaceRegular
        , whitespaceTight
        , zoomControlWidth
        , zoomTrackHeight
        , zoomTrackWidth
        )
import UI.Model exposing (ZoomLevel(..))


type alias Model =
    { zoomLevel : ZoomLevel
    }


type Msg
    = SetZoomLevel Float


initialModel : Model
initialModel =
    { zoomLevel = Far }


zoomLevelToUIValue : ZoomLevel -> Float
zoomLevelToUIValue zoomLevel =
    case zoomLevel of
        VeryFar ->
            1

        Far ->
            2

        Near ->
            3


uiValueToZoomLevel : Float -> ZoomLevel
uiValueToZoomLevel value =
    case floor value of
        1 ->
            VeryFar

        2 ->
            Far

        3 ->
            Near

        _ ->
            Far


update : Msg -> Model -> ( Model, ZoomLevel )
update msg model =
    case msg of
        SetZoomLevel level ->
            let
                zoomLevel =
                    uiValueToZoomLevel level
            in
            ( { model | zoomLevel = zoomLevel }, zoomLevel )


view : Model -> Element Msg
view model =
    let
        baseWidth =
            zoomControlWidth

        baseHeight =
            zoomTrackHeight

        paddingX =
            whitespaceTight

        paddingY =
            whitespaceTight

        sliderWidth =
            baseWidth - (2 * paddingX)

        sliderHeight =
            baseHeight - (2 * paddingY)

        thumbWidth =
            zoomTrackWidth + (2 * paddingX)

        thumbHeight =
            zoomTrackWidth
    in
    Element.el
        [ Element.paddingXY paddingX paddingY
        , Element.width (Element.px baseWidth)
        , Element.height (Element.px baseHeight)
        , Element.alignLeft
        , Element.alignBottom
        , Background.color colorMenuBackground
        , Border.rounded 10
        ]
        (Input.slider
            [ Element.width (Element.px sliderWidth)
            , Element.height (Element.px sliderHeight)
            , Element.behindContent track
            ]
            { onChange = SetZoomLevel
            , label = Input.labelHidden "Zoom"
            , min = 1
            , max = 3
            , step = Just 1
            , value = zoomLevelToUIValue model.zoomLevel
            , thumb =
                Input.thumb
                    [ Element.width (Element.px thumbWidth)
                    , Element.height (Element.px thumbHeight)
                    , Background.color colorZoomThumbBackground
                    , Border.rounded 10
                    , Border.solid
                    , Border.width borderSize
                    , Border.color colorBorder
                    ]
            }
        )


track : Element Msg
track =
    let
        stepGuide =
            Element.el
                [ Element.width Element.fill
                , Element.height (Element.px whitespaceRegular)
                , Background.color colorZoomStepGuide
                ]
                Element.none
    in
    Element.el
        [ Element.width (Element.px zoomTrackWidth)
        , Element.height Element.fill
        , Element.centerX
        , Element.clip
        , Background.color colorZoomTrackBackground
        , Border.solid
        , Border.width borderSize
        , Border.color colorBorder
        , Border.rounded 10
        , Element.inFront
            (Element.column
                [ Element.height Element.fill
                , Element.width Element.fill
                , Element.spaceEvenly
                ]
                [ stepGuide
                , stepGuide
                , stepGuide
                ]
            )
        ]
        Element.none
