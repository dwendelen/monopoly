{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module View where

import Yesod
import Game
import Board
import Ground
import Player
import Data.Vector

import Debug.Trace

mapGame :: Game -> StateView
mapGame game =
  let
    players = Data.Vector.imap (\i p -> mapPlayer i game p) (Game.players game)
    grounds = Data.Vector.map mapGround (Board.grounds . Game.board $ game)
    state = Game.state game
  in
    StateView {
        players = Data.Vector.toList players,
        grounds =  Data.Vector.toList  grounds,
        state = state
      }

mapPlayer idx game player =
  PlayerView {
    playerName = Player.name player,
    position = Player.position player,
    money = Player.money player,
    debt = Player.debt player,
    assets = getAssets idx (Game.board game)
  }

mapGround ground =
  let
    gg = (GroundView {
      groundName = (Ground.getGroundName ground),
      value = (Ground.getCurrentValueOrInitial ground)
    })
  in
    trace ("grrr" Prelude.++ show gg) gg

data StateView = StateView {
  grounds :: [GroundView],
  players :: [PlayerView],
  state :: State
} --deriving (Show)

instance ToJSON StateView where
    toJSON StateView {..} = object
        [ "grounds" .= grounds
        , "players" .= players
        , "state" .= state
        ]

data GroundView = GroundView {
  groundName :: String,
  value :: Maybe Int
} deriving (Show)

instance ToJSON GroundView where
    toJSON GroundView {..} = object
        [ "name" .= groundName
        , "value"  .= value
        ]

data PlayerView = PlayerView {
  playerName :: String,
  position :: Int,
  money :: Int,
  assets :: Int,
  debt :: Int
} deriving (Show)

instance ToJSON PlayerView where
    toJSON PlayerView {..} = object
        [ "name" .= playerName
        , "position"  .= position
        , "money" .= money
        , "assets" .= assets
        , "debt" .= debt
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

addPlayersDummy :: String -> State -> String
addPlayersDummy str _  = str