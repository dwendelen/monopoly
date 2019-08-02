module Ground where

import Data.Maybe(fromMaybe)

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

getGroundName :: Ground -> String
getGroundName FreeParking = "Free Parking"
getGroundName OwnableGround { name = name_} = name_
getGroundName ExtraTax { name = name_} = name_
getGroundName Station { name = name_ } = name_
getGroundName Utility { name = name_ } = name_

getCurrentValueOrInitial :: Ground -> Maybe Int
getCurrentValueOrInitial OwnableGround {currentValue = cval, baseValue = bval} = Just (fromMaybe bval cval)
getCurrentValueOrInitial Station {currentValue = cval } = Just (fromMaybe stationCost cval)
getCurrentValueOrInitial Utility {currentValue = cval } = Just (fromMaybe utilityCost cval)
getCurrentValueOrInitial _ = Nothing

getOwner OwnableGround { owner = owner_} = owner_
getOwner Station { owner = owner_} = owner_
getOwner Utility { owner = owner_} = owner_
getOwner _ = Nothing