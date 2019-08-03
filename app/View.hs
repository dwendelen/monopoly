{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE DeriveGeneric #-}


module View where

import Yesod
import Game
import Ground
import Player
import Types
import Data.Vector

import GHC.Generics


mapGame :: Game -> GameView
mapGame game =
  let
    players = Data.Vector.imap (\i _ -> mapPlayer i game) (Game.players game)
    grounds = Data.Vector.map mapGround (Game.grounds game)
    state = Game.state game
  in
    GameView {
        players = Data.Vector.toList players,
        grounds =  Data.Vector.toList  grounds,
        state =  StateView state,
        economy = Game.economy game,
        logs = Game.logs game
      }

mapPlayer :: PlayerId -> Game -> PlayerView
mapPlayer idx game =
  let
    player = Game.getPlayer idx game
  in
    PlayerView {
      playerName = Player.name player,
      position = Player.position player,
      money = Player.money player,
      debt = Player.debt player,
      assets = getAssets idx game,
      startMoney = calculateStartMoney idx game
    }

mapGround :: Ground -> GroundView
mapGround ground =
  GroundView {
      groundName = Ground.getGroundName ground,
      value = Ground.getCurrentValueOrInitial ground,
      owner = Ground.getOwner ground,
      color = Ground.getColor ground
    }


data GameView = GameView {
  grounds :: [GroundView],
  players :: [PlayerView],
  state :: StateView,
  economy :: Int,
  logs :: [String]
} --deriving (Show)

instance ToJSON GameView where
    toJSON GameView {..} = object
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

newtype StateView = StateView State
instance ToJSON StateView where
  toJSON (StateView AddPlayers) = object
        [ "type" .= String "AddPlayers"  ]
  toJSON (StateView DiceRoll {..})  = object
        [
         "type" .= String "DiceRoll",
         "player" .= player
        ]
  toJSON (StateView BuyOrNot {..}) = object
        [
         "type" .= String "BuyOrNot",
         "player" .= player
        ]



data RollRTO = RollRTO {
  roll :: Int,
  rollPlayer :: Int
} deriving (Generic, Show)

instance FromJSON RollRTO where

newtype PlayerOnly = PlayerOnly {
  player :: Int
} deriving (Generic, Show)

instance FromJSON PlayerOnly where

data PayBackRTO = PayBackRTO {
  payBackPlayer :: Int,
  payBackAmount :: Int
} deriving (Generic, Show)

instance FromJSON PayBackRTO where