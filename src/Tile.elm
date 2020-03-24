module Tile exposing (..)

import Collage exposing (Collage, square, styled, uniform)
import Collage.Layout as Layout
import Color
import Direction exposing (Direction(..))
import Graphics exposing (texture)
import TrafficLight exposing (TrafficLight)


type RoadKind
    = Horizontal
    | Vertical
    | NECorner
    | NWCorner
    | SECorner
    | SWCorner
    | NDeadend
    | SDeadend
    | WDeadend
    | EDeadend


type IntersectionControl
    = Signal TrafficLights
    | Yield
    | Stop
    | Uncontrolled


type IntersectionShape
    = T Direction
    | Crossroads


type alias TrafficLights =
    List TrafficLight


type Tile
    = TwoLaneRoad RoadKind
    | Intersection IntersectionControl IntersectionShape
    | Terrain
    | Empty


trafficLightsAllowEntry : TrafficLights -> Direction -> Bool
trafficLightsAllowEntry trafficLights entryDirection =
    let
        signalXsEntryAllowed tl =
            TrafficLight.isGreen tl && tl.facing == entryDirection
    in
    List.any signalXsEntryAllowed trafficLights


isRoad : Tile -> Bool
isRoad tile =
    case tile of
        TwoLaneRoad _ ->
            True

        _ ->
            False


update : Tile -> Tile
update tile =
    case tile of
        Intersection (Signal trafficLights) shape ->
            let
                next =
                    trafficLights
                        |> List.map TrafficLight.update
                        |> Signal
            in
            Intersection next shape

        _ ->
            tile


view : Float -> Tile -> Collage msg
view tileSize tile =
    let
        ground color =
            square tileSize
                |> styled ( uniform color, Collage.invisible )

        intersection shape content =
            Layout.stack (content ++ [ texture tileSize (intersectionAsset shape) ])

        intersectionWithTrafficLights shape color =
            Direction.horizontal
                |> List.map (Graphics.border tileSize color)
                |> intersection shape
    in
    case tile of
        TwoLaneRoad kind ->
            roadAsset kind
                |> texture tileSize

        Intersection (Signal trafficLights) shape ->
            trafficLights
                |> List.map (TrafficLight.view tileSize)
                |> intersection shape

        Intersection Yield shape ->
            intersectionWithTrafficLights shape Color.lightYellow

        Intersection Stop shape ->
            intersectionWithTrafficLights shape Color.lightRed

        Intersection Uncontrolled shape ->
            intersection shape []

        Terrain ->
            ground (Color.rgb255 33 191 154)

        Empty ->
            ground Color.yellow


intersectionAsset : IntersectionShape -> String
intersectionAsset shape =
    case shape of
        T Up ->
            "XSTN.png"

        T Down ->
            "XSTS.png"

        T Right ->
            "XSTE.png"

        T Left ->
            "XSTW.png"

        Crossroads ->
            "XSX.png"


roadAsset : RoadKind -> String
roadAsset kind =
    case kind of
        Horizontal ->
            "R2LH.png"

        Vertical ->
            "R2LV.png"

        NECorner ->
            "RCNE.png"

        NWCorner ->
            "RCNW.png"

        SECorner ->
            "RCSE.png"

        SWCorner ->
            "RCSW.png"

        NDeadend ->
            "R2LDEN.png"

        SDeadend ->
            "R2LDES.png"

        WDeadend ->
            "R2LDEW.png"

        EDeadend ->
            "R2LDEE.png"
