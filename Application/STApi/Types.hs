module Application.STApi.Types where

import Application.STApi.Prelude
import GHC.Generics

data Agent = Agent
  { accountId :: String,
    symbol :: String,
    headquarters :: String,
    credits :: Int,
    startingFaction :: String,
    shipCount :: Int
  }
  deriving (Show, Generic)

instance FromJSON Agent

data ContractTermsPayment = ContractTermsPayment
  { onAccepted :: Int,
    onFulfilled :: Int
  }
  deriving (Show, Generic)

instance FromJSON ContractTermsPayment

data ContractTermsDeliver = ContractTermsDeliver
  { tradeSymbol :: String,
    destinationSymbol :: String,
    unitsRequired :: Int,
    unitsFulfilled :: Int
  }
  deriving (Show, Generic)

instance FromJSON ContractTermsDeliver

data ContractTerms = ContractTerms
  { deadline :: String,
    payment :: ContractTermsPayment,
    deliver :: [ContractTermsDeliver]
  }
  deriving (Show, Generic)

instance FromJSON ContractTerms

data Contract = Contract
  { id :: String,
    factionSymbol :: String,
    contractType :: String,
    terms :: ContractTerms,
    accepted :: Bool,
    fulfilled :: Bool,
    deadlineToAccept :: String
  }
  deriving (Show, Generic)

instance FromJSON Contract where
  parseJSON =
    genericParseJSON
      defaultOptions
        { fieldLabelModifier = mapper
        }
    where
      mapper "contractType" = "type"
      mapper s = s

data ShipRole
  = FABRICATOR
  | HARVESTER
  | HAULER
  | INTERCEPTOR
  | EXCAVATOR
  | TRANSPORT
  | REPAIR
  | SURVEYOR
  | COMMAND
  | CARRIER
  | PATROL
  | SATELLITE
  | EXPLORER
  deriving (Show, Generic)

instance FromJSON ShipRole

data ShipRegistration = ShipRegistration
  { name :: String,
    factionSymbol :: String,
    role :: ShipRole
  }
  deriving (Show, Generic)

instance FromJSON ShipRegistration

data ShipStatus = IN_TRANSIT | IN_ORBIT | DOCKED deriving (Show, Generic)

instance FromJSON ShipStatus

data ShipFlightMode = DRIFT | STEALTH | CRUISE | BURN deriving (Show, Generic)

instance FromJSON ShipFlightMode

data ShipRoutePoint = ShipRoutePoint
  { symbol :: String,
    waypointType :: WaypointType,
    systemSymbol :: String,
    x :: Int,
    y :: Int
  }
  deriving (Show, Generic)

instance FromJSON ShipRoutePoint where
  parseJSON =
    genericParseJSON
      defaultOptions
        { fieldLabelModifier = mapper
        }
    where
      mapper "waypointType" = "type"
      mapper s = s

data ShipRoute = ShipRoute
  { departureTime :: String,
    arrival :: String,
    destination :: ShipRoutePoint,
    origin :: ShipRoutePoint
  }
  deriving (Show, Generic)

instance FromJSON ShipRoute

data ShipNav = ShipNav
  { systemSymbol :: String,
    waypointSymbol :: String,
    status :: ShipStatus,
    flightMode :: ShipFlightMode,
    route :: ShipRoute
  }
  deriving (Show, Generic)

instance FromJSON ShipNav

data ShipCargoInventory = ShipCargoInventory
  { symbol :: String,
    name :: String,
    description :: String,
    units :: Int
  }
  deriving (Show, Generic)

instance FromJSON ShipCargoInventory

data ShipCargo = ShipCargo
  { capacity :: Int,
    units :: Int,
    inventory :: [ShipCargoInventory]
  }
  deriving (Show, Generic)

instance FromJSON ShipCargo

data ShipFuelConsumed = ShipFuelConsumed {amount :: Int, timestamp :: String} deriving (Show, Generic)

instance FromJSON ShipFuelConsumed

data ShipFuel = ShipFuel
  { current :: Int,
    capacity :: Int,
    consumed :: ShipFuelConsumed
  }
  deriving (Show, Generic)

instance FromJSON ShipFuel

data ShipCooldown = ShipCooldown
  { shipSymbol :: String,
    totalSeconds :: Int,
    remainingSeconds :: Int,
    expiration :: Maybe String
  }
  deriving (Show, Generic)

instance FromJSON ShipCooldown

data Ship = Ship
  { symbol :: String,
    registration :: ShipRegistration,
    nav :: ShipNav,
    cargo :: ShipCargo,
    fuel :: ShipFuel,
    cooldown :: ShipCooldown
  }
  deriving (Show, Generic)

instance FromJSON Ship

data WaypointType = PLANET | GAS_GIANT | MOON | ORBITAL_STATION | JUMP_GATE | ASTEROID_FIELD | ASTEROID | ENGINEERED_ASTEROID | ASTEROID_BASE | NEBULA | DEBRIS_FIELD | GRAVITY_WELL | ARTIFICIAL_GRAVITY_WELL | FUEL_STATION deriving (Show, Generic)

instance FromJSON WaypointType
