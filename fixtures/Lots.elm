module Lots exposing
    ( oneByOneLot
    , twoByTwoLot
    )

import Model.Lot as Lot exposing (Lot)
import Model.Tilemap exposing (Cell, OrthogonalDirection(..), tileSize)
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
    Lot.build oneByOneNewLot ( 1, 2 )


twoByTwoNewLot : Lot.NewLot
twoByTwoNewLot =
    { content =
        { kind = Lot.ResidentialE
        , entryDirection = Down
        }
    , width = tileSize |> Quantity.multiplyBy 2
    , height = tileSize |> Quantity.multiplyBy 2
    }


twoByTwoLot : Cell -> Lot
twoByTwoLot anchorCell =
    Lot.build twoByTwoNewLot anchorCell
