module Model.Route exposing
    ( Parking
    , Route
    , description
    , unrouted
    )

import Duration exposing (Duration)
import Model.Entity exposing (Id)
import Model.RoadNetwork exposing (RNNodeContext)


type alias Route =
    { connections : List RNNodeContext
    , parking : Maybe Parking
    }


type alias Parking =
    { lotId : Id
    , parkingSpotId : Id
    , waitTimer : Maybe Duration
    , lockAvailable : Bool
    }


unrouted : Route
unrouted =
    { connections = []
    , parking = Nothing
    }


description : Route -> String
description route =
    case route.connections of
        target :: _ ->
            "(target node: " ++ String.fromInt target.node.id ++ ")"

        _ ->
            "(no route)"
