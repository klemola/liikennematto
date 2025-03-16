module Data.Lots exposing
    ( NewLot
    , ParkingRestriction(..)
    , allLots
    , drivewayOffset
    , fireStation
    , residentialApartments1
    , residentialRow1
    , residentialSingle1
    , school
    )

import Common exposing (LocalCoordinates)
import Data.Cars as Cars exposing (CarMake)
import Data.Colors as Colors
import Length exposing (Length)
import Lib.OrthogonalDirection exposing (OrthogonalDirection(..))
import Point2d exposing (Point2d)
import Quantity
import Tilemap.Cell as Cell


type alias NewLot =
    { name : String
    , horizontalTilesAmount : Int
    , verticalTilesAmount : Int
    , parkingSpotExitDirection : OrthogonalDirection
    , parkingSpots : List ( Point2d Length.Meters LocalCoordinates, ParkingRestriction )
    , entryDirection : OrthogonalDirection
    , parkingLaneStartPosition : Point2d Length.Meters LocalCoordinates
    , parkingLaneStartDirection : OrthogonalDirection
    , residents : List CarMake
    }


type ParkingRestriction
    = ResidentParkingOnly
    | NoRestriction


allLots : List NewLot
allLots =
    [ residentialSingle1
    , residentialRow1
    , residentialApartments1
    , school
    , cafe
    , fireStation
    , park1
    , concertVenue
    , flowerShop
    ]


drivewayOffset : Length
drivewayOffset =
    -- the offset of lot entry tiles' driveway (vs. center-positioned road)
    Cell.size |> Quantity.multiplyBy 0.2


residentParking : Point2d Length.Meters LocalCoordinates -> ( Point2d Length.Meters LocalCoordinates, ParkingRestriction )
residentParking position =
    ( position, ResidentParkingOnly )


noRestrictions : Point2d Length.Meters LocalCoordinates -> ( Point2d Length.Meters LocalCoordinates, ParkingRestriction )
noRestrictions position =
    ( position, NoRestriction )


residentialSingle1 : NewLot
residentialSingle1 =
    { name = "LotResidentialSingle1"
    , horizontalTilesAmount = 2
    , verticalTilesAmount = 2
    , parkingSpotExitDirection = Right
    , parkingSpots =
        [ residentParking <| Point2d.fromMeters { x = 21.25, y = 3.75 }
        ]
    , entryDirection = Left
    , parkingLaneStartPosition = Point2d.fromMeters { x = 21.25, y = 3.75 }
    , parkingLaneStartDirection = Left
    , residents =
        [ Cars.sedan Colors.red Colors.redDarker
        ]
    }


school : NewLot
school =
    { name = "LotSchool"
    , horizontalTilesAmount = 3
    , verticalTilesAmount = 3
    , parkingSpotExitDirection = Down
    , parkingSpots =
        [ noRestrictions <| Point2d.fromMeters { x = 9.85, y = 9.125 }
        , noRestrictions <| Point2d.fromMeters { x = 14.375, y = 9.125 }
        , noRestrictions <| Point2d.fromMeters { x = 18.812, y = 9.125 }
        , noRestrictions <| Point2d.fromMeters { x = 23.375, y = 9.125 }
        , noRestrictions <| Point2d.fromMeters { x = 27.875, y = 9.125 }
        ]
    , entryDirection = Right
    , parkingLaneStartPosition = Point2d.fromMeters { x = 7.5, y = 3.75 }
    , parkingLaneStartDirection = Right
    , residents =
        [ Cars.van Colors.orange Colors.orangeDarker
        ]
    }


cafe : NewLot
cafe =
    { name = "LotCafe"
    , horizontalTilesAmount = 2
    , verticalTilesAmount = 2
    , parkingSpotExitDirection = Down
    , parkingSpots =
        [ residentParking <| Point2d.fromMeters { x = 3.875, y = 9.125 }
        , noRestrictions <| Point2d.fromMeters { x = 8.375, y = 9.125 }
        , noRestrictions <| Point2d.fromMeters { x = 12.8125, y = 9.125 }
        , noRestrictions <| Point2d.fromMeters { x = 17.375, y = 9.125 }
        , noRestrictions <| Point2d.fromMeters { x = 21.875, y = 9.125 }
        ]
    , entryDirection = Left
    , parkingLaneStartPosition = Point2d.fromMeters { x = 24.5, y = 3.75 }
    , parkingLaneStartDirection = Left
    , residents =
        [ Cars.van Colors.lightBrown Colors.lightBrownDarker
        ]
    }


residentialRow1 : NewLot
residentialRow1 =
    { name = "LotResidentialRow1"
    , horizontalTilesAmount = 3
    , verticalTilesAmount = 2
    , parkingSpotExitDirection = Down
    , parkingSpots =
        [ noRestrictions <| Point2d.fromMeters { x = 9.75, y = 9.125 }
        , noRestrictions <| Point2d.fromMeters { x = 14.25, y = 9.125 }
        , noRestrictions <| Point2d.fromMeters { x = 18.6875, y = 9.125 }
        , residentParking <| Point2d.fromMeters { x = 23.25, y = 9.125 }
        ]
    , entryDirection = Right
    , parkingLaneStartPosition = Point2d.fromMeters { x = 7.5, y = 3.75 }
    , parkingLaneStartDirection = Right
    , residents =
        [ Cars.sedan Colors.darkBlue Colors.darkBlueDarker
        , Cars.hatchback Colors.darkBlue Colors.darkBlueDarker
        ]
    }


residentialApartments1 : NewLot
residentialApartments1 =
    { name = "LotResidentialApartments1"
    , horizontalTilesAmount = 2
    , verticalTilesAmount = 3
    , parkingSpotExitDirection = Down
    , parkingSpots =
        [ noRestrictions <| Point2d.fromMeters { x = 3.125, y = 12.25 }
        , noRestrictions <| Point2d.fromMeters { x = 7.75, y = 12.25 }
        , residentParking <| Point2d.fromMeters { x = 12.375, y = 12.25 }
        ]
    , entryDirection = Up
    , parkingLaneStartPosition = Point2d.fromMeters { x = 4.75, y = 7.1 }
    , parkingLaneStartDirection = Up
    , residents =
        [ Cars.hatchback Colors.yellowGreen Colors.yellowGreenDarker
        , Cars.sedan Colors.yellowGreen Colors.yellowGreenDarker
        ]
    }


fireStation : NewLot
fireStation =
    { name = "LotFireStation"
    , horizontalTilesAmount = 3
    , verticalTilesAmount = 2
    , parkingSpotExitDirection = Left
    , parkingSpots =
        [ residentParking <| Point2d.fromMeters { x = 20.4, y = 5 }
        ]
    , entryDirection = Up
    , parkingLaneStartPosition = Point2d.fromMeters { x = 12, y = 5 }
    , parkingLaneStartDirection = Right
    , residents =
        [ Cars.fireTruck
        ]
    }


park1 : NewLot
park1 =
    { name = "LotPark1"
    , horizontalTilesAmount = 3
    , verticalTilesAmount = 3
    , parkingSpotExitDirection = Down
    , parkingSpots =
        [ noRestrictions <| Point2d.fromMeters { x = 6.5, y = 9.875 }
        ]
    , entryDirection = Right
    , parkingLaneStartPosition = Point2d.fromMeters { x = 6.5, y = 9.875 }
    , parkingLaneStartDirection = Up
    , residents = []
    }


concertVenue : NewLot
concertVenue =
    { name = "LotConcertVenue"
    , horizontalTilesAmount = 3
    , verticalTilesAmount = 2
    , parkingSpotExitDirection = Down
    , parkingSpots =
        [ noRestrictions <| Point2d.fromMeters { x = 18.875, y = 10.3125 }
        , noRestrictions <| Point2d.fromMeters { x = 23.25, y = 10.3125 }
        ]
    , entryDirection = Up
    , parkingLaneStartPosition = Point2d.fromMeters { x = 8.5625, y = 5 }
    , parkingLaneStartDirection = Right
    , residents = []
    }


flowerShop : NewLot
flowerShop =
    { name = "LotFlowerShop"
    , horizontalTilesAmount = 2
    , verticalTilesAmount = 3
    , parkingSpotExitDirection = Down
    , parkingSpots =
        [ noRestrictions <| Point2d.fromMeters { x = 3.125, y = 10.53125 }
        , residentParking <| Point2d.fromMeters { x = 7.75, y = 10.53125 }
        ]
    , entryDirection = Up
    , parkingLaneStartPosition = Point2d.fromMeters { x = 4.75, y = 7.1 }
    , parkingLaneStartDirection = Up
    , residents =
        [ Cars.sedan Colors.pink Colors.pinkDarker
        ]
    }
