module Ground where

stationCost :: Int
stationCost = 200

utilityCost :: Int
utilityCost = 150


data Ground
  = FreeParking
  | OwnableGround
      { name :: String
      , baseValue :: Int
      , currentValue :: Maybe Int
      , rent :: [Int]
      , owner :: Maybe Int
      }
  | ExtraTax { name :: String, tax :: Int }
  | Station
      { name :: String
      , owner :: Maybe Int
      , currentValue :: Maybe Int
      }
  | Utility
      { name :: String
      , owner :: Maybe Int
      , currentValue :: Maybe Int
      }
