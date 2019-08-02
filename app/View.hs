{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE DeriveGeneric #-}


module View where

import Yesod
import Game
import Ground
import Player
import Data.Vector

import Debug.Trace
import GHC.Generics


mapGame :: Game -> StateView
mapGame game =
  let
    players = Data.Vector.imap (\i p -> mapPlayer i game p) (Game.players game)
    grounds = Data.Vector.map mapGround (Game.grounds game)
    state = Game.state game
  in
    StateView {
        players = Data.Vector.toList players,
        grounds =  Data.Vector.toList  grounds,
        state = state,
        economy = Game.economy game,
        logs = Game.logs game
      }

mapPlayer idx game player =
  PlayerView {
    playerName = Player.name player,
    position = Player.position player,
    money = Player.money player,
    debt = Player.debt player,
    assets = getAssets idx game,
    startMoney = calculateStartMoney idx game
  }

mapGround ground =
  GroundView {
      groundName = (Ground.getGroundName ground),
      value = (Ground.getCurrentValueOrInitial ground),
      owner = (Ground.getOwner ground),
      color = (Ground.getColor ground)
    }


data StateView = StateView {
  grounds :: [GroundView],
  players :: [PlayerView],
  state :: State,
  economy :: Int,
  logs :: [String]
} --deriving (Show)

instance ToJSON StateView where
    toJSON StateView {..} = object
        [ "grounds" .= grounds
        , "players" .= players
        , "state" .= state
        , "economy" .= economy
        , "logs" .= logs
        ]

data GroundView = GroundView {
  groundName :: String,
  value :: Maybe Int,
  owner :: Maybe Int,
  color :: Maybe String
} deriving (Show)

instance ToJSON GroundView where
    toJSON GroundView {..} = object
        [ "name" .= groundName
        , "value"  .= value
        , "owner" .= owner
        , "color" .= color
        ]

data PlayerView = PlayerView {
  playerName :: String,
  position :: Int,
  money :: Int,
  assets :: Int,
  debt :: Int,
  startMoney :: Int
} deriving (Show)

instance ToJSON PlayerView where
    toJSON PlayerView {..} = object
        [ "name" .= playerName
        , "position"  .= position
        , "money" .= money
        , "assets" .= assets
        , "debt" .= debt
        , "startMoney" .= startMoney
        ]

instance ToJSON State where
  toJSON AddPlayers = object
        [ "type" .= String "AddPlayers"  ]
  toJSON DiceRoll {..}  = object
        [
         "type" .= String "DiceRoll",
         "player" .= player
        ]
  toJSON BuyOrNot {..} = object
        [
         "type" .= String "BuyOrNot",
         "player" .= player
        ]



data RollRTO = RollRTO {
  roll :: Int,
  rollPlayer :: Int
} deriving (Generic, Show)

instance FromJSON RollRTO where

data PlayerOnly = PlayerOnly {
  player :: Int
} deriving (Generic, Show)

instance FromJSON PlayerOnly where