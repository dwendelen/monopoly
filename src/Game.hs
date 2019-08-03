module Game where

import Data.Vector (empty, Vector, snoc, length, map, (!), (//))

import Ground
import Player
import Types
import Data.Maybe

baseStartMoney :: Amount
baseStartMoney = 200

data Game = Game
  { players :: Vector Player
  , grounds :: Vector Ground
  , state :: State
  , interestRate :: Float
  , returnRate :: Float
  , economy :: Amount
  , logs :: [String]
  }

data State
  = AddPlayers
  | DiceRoll {player :: PlayerId }
  | BuyOrNot {player:: PlayerId }

initialGame :: Vector Ground -> Float -> Float -> Game
initialGame initialBoard interestRate_ returnRate_ = Game
  { players = Data.Vector.empty
  , grounds = initialBoard
  , state = AddPlayers
  , interestRate = interestRate_
  , returnRate = returnRate_
  , economy = 0
  , logs = []
  }

getAssets :: PlayerId -> Game -> Amount
getAssets pId Game { grounds = _grounds } =
     sum . Data.Vector.map (getAssetOrZero pId) $ _grounds

getAssetOrZero :: PlayerId -> Ground -> Amount
getAssetOrZero pId ground =
    case Ground.getOwner ground of
      Nothing -> 0
      Just owner__ ->
        if owner__ == pId
          then fromJust $ getCurrentValueOrInitial ground
          else 0


calculateStartMoney :: PlayerId -> Game -> Amount
calculateStartMoney pIdx game =
  let
    debt_ = Player.debt $ getPlayer pIdx game
    returnFromEco = Game.returnRate game * fromIntegral (Game.economy game)
    interest = Game.interestRate game * fromIntegral debt_
  in
    round (returnFromEco + fromIntegral baseStartMoney - interest)

addPlayer :: String -> Game -> Game
addPlayer name_ game =
  let
    newPlayer = Player { Player.name = name_, debt = 0, money = 0, position = 0 }
    newPlayers = snoc (players game) newPlayer
  in
    game { players = newPlayers }

startGame :: Game -> Game
startGame game =
    game { state = DiceRoll 0 }

getPlayer :: PlayerId -> Game -> Player
getPlayer pId game =
  players game ! pId

getPlayerPosition :: PlayerId -> Game -> Position
getPlayerPosition pId game =
  position $ getPlayer pId game

getPlayerGround :: PlayerId -> Game -> Ground
getPlayerGround pId game =
  grounds game ! getPlayerPosition pId game

getPlayerGroundName :: PlayerId -> Game -> String
getPlayerGroundName pId =
  Ground.getGroundName . getPlayerGround pId

getPlayerName :: PlayerId -> Game -> String
getPlayerName pId
  = Player.name . getPlayer pId

movePlayer :: Steps -> PlayerId -> Game -> (Game, Bool)
movePlayer amount pId game =
  let
    nbOfGrounds = Data.Vector.length . grounds $ game
    gameAfterMove = updatePlayer (Player.move amount nbOfGrounds) pId game

    oldPos = getPlayerPosition pId game
    newPos = getPlayerPosition pId gameAfterMove
    camePastStart = newPos < oldPos
  in
    (gameAfterMove, camePastStart)

updatePlayer :: (Player -> Player) -> PlayerId -> Game -> Game
updatePlayer fn pId game =
  let
    player_ = getPlayer pId game
    newPlayer = fn player_
    newPlayers = players game Data.Vector.// [(pId, newPlayer)]
  in
    game { players = newPlayers }

addToEconomy :: Amount -> Game -> Game
addToEconomy amount game =
  game { economy = economy game + amount }

fromEconomyToPlayer :: Amount -> PlayerId -> Game -> Game
fromEconomyToPlayer amount pId =
  addToEconomy (- amount) . updatePlayer (Player.addMoney amount) pId

fromPlayerToEconomy :: Amount -> PlayerId -> Game -> Game
fromPlayerToEconomy amount =
  fromEconomyToPlayer (- amount)

addLog :: String -> Game -> Game
addLog msg game =
  let
    oldLogs = Game.logs game
    withExtra = msg : oldLogs
    newLogs = take 100 withExtra
  in
    game {logs = newLogs}

nextPlayer :: PlayerId -> Game -> Game
nextPlayer pId game =
  let
    nbPlayers = Data.Vector.length (players game)
    nextPlayerId = (pId + 1) `mod` nbPlayers
    newState = DiceRoll nextPlayerId
  in
    game {state = newState}

currentPlayer :: Game -> PlayerId
currentPlayer Game { state = DiceRoll {player = pId } } = pId
currentPlayer Game { state = BuyOrNot {player = pId } } = pId
currentPlayer Game { state = AddPlayers } = undefined
-- Should come in logic


-- end should come in logic
buyCash :: PlayerId -> Game -> Game
buyCash pId game =
  let
    player_ = players game ! pId
    pos = Player.position player_
    currentGround = getPlayerGround pId game

    price = fromJust $ getCurrentValueOrInitial currentGround

    boughtGround = currentGround { owner = Just pId }
    newGrounds = (Game.grounds game) // [(pos, boughtGround)]

    playerAfterPaying = player_ { money = money player_ - price }
    newPlayers = (Game.players game) // [(pId, playerAfterPaying)]

    economyAfterPaying = economy game + price

    gameAfterBuying = game { economy = economyAfterPaying, players = newPlayers, grounds = newGrounds}
    gameAfterLogging = addLog (Player.name player_ Prelude.++ " bought " Prelude.++ Ground.getGroundName boughtGround Prelude.++ " with cash for " Prelude.++ show price) gameAfterBuying
  in
    nextPlayer pId gameAfterLogging

buyBorrow :: PlayerId -> Game -> Game
buyBorrow pId game =
  let
    player_ = players game ! pId
    pos = Player.position player_
    currentGround = (Game.grounds game) ! pos

    price = fromJust $ getCurrentValueOrInitial currentGround

    boughtGround = currentGround { owner = Just pId }
    newGrounds = (Game.grounds game) // [(pos, boughtGround)]

    playerAfterPaying = player_ { debt = debt player_ + price }
    newPlayers = (Game.players game) // [(pId, playerAfterPaying)]

    economyAfterPaying = economy game + price

    gameAfterBuying = game { economy = economyAfterPaying, players = newPlayers, grounds = newGrounds}
    gameAfterLogging = addLog (Player.name player_ Prelude.++ " bought " Prelude.++ Ground.getGroundName boughtGround Prelude.++ " with a loan for " Prelude.++ show price) gameAfterBuying
  in
    nextPlayer pId gameAfterLogging

-- todo swap pid
payBack :: Amount -> PlayerId -> Game -> Game
payBack amount pId game =
  let
    player_ = players game ! pId
    newPlayer = player_ { money = money player_ - amount, debt = debt player_ - amount}
    msg = Player.name player_ Prelude.++ " paid back " Prelude.++ show amount
    newPlayers = players game // [(pId, newPlayer)]
  in
    addLog msg $ game {players = newPlayers}

