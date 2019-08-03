module Ground where

import Types

stationCost :: Int
stationCost = 200

utilityCost :: Int
utilityCost = 150

data Ground
  = Start
  | FreeParking
  | OwnableGround
      { name :: String
      , baseValue :: Amount
      , rent :: [Amount]
      , owner :: Maybe PlayerId
      , color :: Color
      }
  | ExtraTax { name :: String, tax :: Amount }
  | Station
      { name :: String
      , owner :: Maybe PlayerId
      }
  | Utility
      { name :: String
      , owner :: Maybe PlayerId
      }

getGroundName :: Ground -> String
getGroundName Start = "Start - Start - Start - Start"
getGroundName FreeParking = "Free Parking"
getGroundName OwnableGround { name = name_} = name_
getGroundName ExtraTax { name = name_} = name_
getGroundName Station { name = name_ } = name_
getGroundName Utility { name = name_ } = name_

getCurrentValueOrInitial :: Ground -> Maybe Amount
getCurrentValueOrInitial OwnableGround { baseValue = bval} = Just  bval
getCurrentValueOrInitial Station {} = Just  stationCost
getCurrentValueOrInitial Utility {} = Just  utilityCost
getCurrentValueOrInitial _ = Nothing

getOwner :: Ground -> Maybe PlayerId
getOwner OwnableGround { owner = owner_} = owner_
getOwner Station { owner = owner_} = owner_
getOwner Utility { owner = owner_} = owner_
getOwner _ = Nothing

getColor :: Ground -> Maybe Color
getColor OwnableGround { color = color_} = Just color_
getColor Station {} = Just "grey"
getColor _ = Nothing

amountToPay :: Steps -> Ground -> Amount
amountToPay _ OwnableGround { rent = rent_} = head rent_
amountToPay roll Utility {} = 4 * roll
amountToPay _ Station {} = 25
amountToPay _ Start = undefined
amountToPay _ FreeParking = undefined
amountToPay _ ExtraTax {} = undefined