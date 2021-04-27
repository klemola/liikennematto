module Lots exposing
    ( createTwoByTwoLot
    , oneByOneLot
    , twoByTwoLot
    )

import Cell exposing (Corner(..), OrthogonalDirection(..))
import Config exposing (tileSizeInMeters)
import Lot exposing (Anchor, Lot)
import Point2d
import Quantity


oneByOneNewLot : Lot.NewLot
oneByOneNewLot =
    { content =
        { kind = Lot.ResidentialA
        , entryDirection = Down
        }
    , width = tileSizeInMeters
    , height = tileSizeInMeters
    }


oneByOneLot : Lot
oneByOneLot =
    let
        anchor =
            ( ( 1, 2 ), Up )
    in
    { content = oneByOneNewLot.content
    , width = oneByOneNewLot.width
    , height = oneByOneNewLot.height
    , position =
        Point2d.xy
            Quantity.zero
            (tileSizeInMeters |> Quantity.multiplyBy 9)
    , entryDetails = Lot.entryDetails anchor oneByOneNewLot
    , anchor = anchor
    }


twoByTwoNewLot : Lot.NewLot
twoByTwoNewLot =
    { content =
        { kind = Lot.ResidentialE
        , entryDirection = Down
        }
    , width = tileSizeInMeters |> Quantity.multiplyBy 2
    , height = tileSizeInMeters |> Quantity.multiplyBy 2
    }


twoByTwoLot : Lot
twoByTwoLot =
    createTwoByTwoLot ( ( 1, 3 ), Up )


createTwoByTwoLot : Anchor -> Lot
createTwoByTwoLot ( anchorCell, anchorDir ) =
    let
        content =
            twoByTwoNewLot.content

        anchor =
            ( anchorCell, anchorDir )
    in
    { content = { content | entryDirection = Cell.oppositeOrthogonalDirection anchorDir }
    , width = twoByTwoNewLot.width
    , height = twoByTwoNewLot.height
    , position =
        anchorCell
            |> Cell.next anchorDir
            |> Cell.bottomLeftCorner
    , entryDetails = Lot.entryDetails anchor twoByTwoNewLot
    , anchor = anchor
    }
