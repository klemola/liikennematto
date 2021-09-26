module Lots exposing
    ( createTwoByTwoLot
    , oneByOneLot
    , twoByTwoLot
    )

import Model.Board as Board exposing (OrthogonalDirection(..), tileSize)
import Model.Lot as Lot exposing (Anchor, Lot)
import Point2d
import Quantity


oneByOneNewLot : Lot.NewLot
oneByOneNewLot =
    { content =
        { kind = Lot.ResidentialA
        , entryDirection = Down
        }
    , width = tileSize
    , height = tileSize
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
            (tileSize |> Quantity.multiplyBy 9)
    , entryDetails = Lot.entryDetails anchor oneByOneNewLot
    , anchor = anchor
    }


twoByTwoNewLot : Lot.NewLot
twoByTwoNewLot =
    { content =
        { kind = Lot.ResidentialE
        , entryDirection = Down
        }
    , width = tileSize |> Quantity.multiplyBy 2
    , height = tileSize |> Quantity.multiplyBy 2
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
    { content = { content | entryDirection = Board.oppositeOrthogonalDirection anchorDir }
    , width = twoByTwoNewLot.width
    , height = twoByTwoNewLot.height
    , position =
        anchorCell
            |> Board.nextOrthogonalCell anchorDir
            |> Board.cellBottomLeftCorner
    , entryDetails = Lot.entryDetails anchor twoByTwoNewLot
    , anchor = anchor
    }
