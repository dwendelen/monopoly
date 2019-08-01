module Game where

stationCost :: Int
stationCost = 200

utilityCost :: Int
utilityCost = 150





data Ground
  = FreeParking Int
  | OwnableGround
      { name :: String
      , baseValue :: Int
      , currentValue :: Maybe Int
      , rent :: [Int]
      , owner :: Maybe String
      }
  | ExtraTax { name :: String, tax :: Int }
  | Station
      { name :: String
      , owner :: Maybe String
      , currentValue :: Maybe Int
      }
  | Utility
      { name :: String
      , owner :: Maybe String
      , currentValue :: Maybe Int
      }


data Board = Board
  { grounds :: [Ground]
  }

data Player = Player
  { name :: Int
  , debt :: Int
  , money :: Int
  , position :: Int
  }

getAssets :: String -> Board -> Int
getAssets player Board { grounds = _grounds } =
    map (getAssetOrZero player) _grounds


getAssetOrZero player OwnableGround { owner = Just player, currentValue = _currentValue } = _currentValue
getAssetOrZero player Station { owner = Just player, currentValue = _currentValue } = _currentValue
getAssetOrZero player Utility { owner = Just player, currentValue = _currentValue } = _currentValue
getAssetOrZero player _ = 0


data Game = Game
  { players :: [Player]
  , board :: Board
  }