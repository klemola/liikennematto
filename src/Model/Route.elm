module Model.Route exposing
    ( Route
    , description
    , unrouted
    )

import Model.Entity exposing (Id)
import Model.RoadNetwork exposing (RNNodeContext)


type alias Route =
    { connections : List RNNodeContext
    , parking :
        Maybe
            { lotId : Id
            , parkingSpotId : Id
            }
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
