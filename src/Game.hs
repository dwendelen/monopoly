module Game where

import Data.Vector (empty, Vector, (++), snoc, length, map, (!), (//))

import Board
import Ground
import Player
import Data.Maybe

getAssets :: Int -> Board -> Int
getAssets player Board { grounds = _grounds } =
     sum . Data.Vector.map (getAssetOrZero player) $ _grounds

getAssetOrZero :: Int -> Ground -> Int
getAssetOrZero player OwnableGround { owner = Just player_, currentValue = Just _currentValue } = if player_ == player then _currentValue else 0
getAssetOrZero player Station { owner = Just player_, currentValue = Just _currentValue } = if player_ == player then _currentValue else 0
getAssetOrZero player Utility { owner = Just player_, currentValue = Just _currentValue } = if player_ == player then _currentValue else 0
getAssetOrZero player _ = 0


data Game = Game
  { players :: Vector Player
  , board :: Board
  , state :: State
  }

data State
  = AddPlayers
  | DiceRoll {player :: Int }
  | BuyOrNot {player:: Int }

initialGame initialBoard = Game
  { players = Data.Vector.empty
  , board = initialBoard
  , state = AddPlayers
  }

addPlayer :: String -> Game -> (Game, [String])
addPlayer name_ game =
  let
    newPlayer = Player { Player.name = name_, debt = 0, money = 0, position = 0 }
    newPlayers = snoc (players game) newPlayer
    newGame = game { players = newPlayers }
  in
    (newGame, [name_ Prelude.++ " joined"])

startGame :: Game -> (Game, [String])
startGame game =
  let
    newState = DiceRoll 0
    newGame = game { state = newState }
  in
    (newGame, ["Game started"])

rollDice :: Int -> Int -> Game -> (Game, [String])
rollDice playerId roll game =
  let
    nbPlaces = Data.Vector.length (grounds . board $ game)
    player = (players game) ! playerId
    oldPosition = position player
    newPosition = (oldPosition + roll) `mod` nbPlaces
    camePastStart = oldPosition < newPosition

    newPlayer = player { position = newPosition }
    newPlayers = players game Data.Vector.// [(playerId, newPlayer)]

    newGame = game {players = newPlayers}
    msg = Player.name player Prelude.++
        " moved " Prelude.++ show roll Prelude.++
        " from " Prelude.++ show oldPosition Prelude.++
        " to " Prelude.++ show newPosition
  in
    (newGame, [msg])

