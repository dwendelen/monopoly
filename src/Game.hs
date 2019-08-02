module Game where

import Data.Vector (empty, Vector, (++), snoc, length, map, (!), (//))

import Ground
import Player
import Data.Maybe

baseStartMoney = 200

getAssets :: Int -> Game -> Int
getAssets player Game { grounds = _grounds } =
     sum . Data.Vector.map (getAssetOrZero player) $ _grounds

getAssetOrZero :: Int -> Ground -> Int
getAssetOrZero player OwnableGround { owner = Just player_, currentValue = Just _currentValue } = if player_ == player then _currentValue else 0
getAssetOrZero player Station { owner = Just player_, currentValue = Just _currentValue } = if player_ == player then _currentValue else 0
getAssetOrZero player Utility { owner = Just player_, currentValue = Just _currentValue } = if player_ == player then _currentValue else 0
getAssetOrZero player _ = 0

calculateStartMoney :: Int -> Game -> Int
calculateStartMoney pIdx game =
  let
    debt = Player.debt (players game ! pIdx)
    returnFromEco = (Game.returnRate game) * fromIntegral (Game.economy game)
    interest = (Game.interestRate game) * fromIntegral debt
  in
    round (returnFromEco + fromIntegral baseStartMoney - interest)

data Game = Game
  { players :: Vector Player
  , grounds :: Vector Ground
  , state :: State
  , interestRate :: Float
  , returnRate :: Float
  , economy :: Int
  , logs :: [String]
  }

data State
  = AddPlayers
  | DiceRoll {player :: Int }
  | BuyOrNot {player:: Int }

initialGame initialBoard interestRate returnRate = Game
  { players = Data.Vector.empty
  , grounds = initialBoard
  , state = AddPlayers
  , interestRate = interestRate
  , returnRate = returnRate
  , economy = 0
  , logs = []
  }

addPlayer :: String -> Game -> Game
addPlayer name_ game =
  let
    newPlayer = Player { Player.name = name_, debt = 0, money = 0, position = 0 }
    newPlayers = snoc (players game) newPlayer
    newGame = game { players = newPlayers }
  in
    addLog (name_ Prelude.++ " joined") newGame

startGame :: Game -> Game
startGame game =
  let
    newState = DiceRoll 0
    newGame = game { state = newState }
  in
    addLog "Game started" newGame

rollDice :: Int -> Int -> Game -> Game
rollDice playerId roll game =
  let
    nbPlaces = Data.Vector.length (grounds $ game)

    player = players game ! playerId
    oldPosition = position player
    newPosition = (oldPosition + roll) `mod` nbPlaces
    camePastStart = newPosition < oldPosition

    newPlayer = player { position = newPosition }
    newPlayers = players game Data.Vector.// [(playerId, newPlayer)]

    oldGroundName = Ground.getGroundName $ grounds game ! oldPosition
    newGroundName = Ground.getGroundName $ grounds game ! newPosition

    msg = Player.name player Prelude.++
        " moved " Prelude.++ show roll Prelude.++
        " from " Prelude.++ oldGroundName Prelude.++
        " to " Prelude.++ newGroundName

    gameAfterMovingPlayer = addLog msg $ game { players = newPlayers }
    gameAfterStartMoney = if camePastStart
          then applyStartMoney gameAfterMovingPlayer playerId
          else gameAfterMovingPlayer

    gameAfterHandling = handleLanding roll gameAfterStartMoney playerId
  in
    gameAfterHandling

applyStartMoney :: Game -> Int -> Game
applyStartMoney game playerId =
  let
    amount = calculateStartMoney playerId game
    player = players game ! playerId
    playerAfterPaying = player { money = money player + amount}
    economyAfterPaying = economy game - amount
    newPlayers = players game Data.Vector.// [(playerId, playerAfterPaying)]
    gameAfterUpdates = game { players = newPlayers, economy = economyAfterPaying }
    msg = Player.name player Prelude.++ " received " Prelude.++ show amount Prelude.++ " start money"
  in
    addLog msg gameAfterUpdates

handleLanding :: Int -> Game -> Int -> Game
handleLanding roll game playerId =
  let
    player = players game ! playerId
    pos = Player.position player

    currentGround = (Game.grounds game) ! pos
  in
    handleLandedOnGround roll game playerId currentGround

handleLandedOnGround :: Int -> Game -> Int -> Ground -> Game
handleLandedOnGround _ game playerId FreeParking =
  nextPlayer game playerId
handleLandedOnGround _ game playerId ExtraTax {} =
  nextPlayer game playerId --todo

handleLandedOnGround _ game playerId OwnableGround {  owner = Nothing  } =
  game { state = BuyOrNot playerId }
handleLandedOnGround _ game playerId OwnableGround {  rent = rent , owner = Just owner  } =
  let
    gameAfterPay = pay playerId owner (head rent) game
    gameToContinueFrom = if playerId == owner then game else gameAfterPay
  in
    nextPlayer gameToContinueFrom playerId

handleLandedOnGround _ game playerId Utility {  owner = Nothing  } =
  game { state = BuyOrNot playerId }
handleLandedOnGround roll game playerId Utility { owner = Just owner  } =
  let
    gameAfterPay = pay playerId owner (4 * roll) game
    gameToContinueFrom = if playerId == owner then game else gameAfterPay
  in
    nextPlayer gameToContinueFrom playerId

handleLandedOnGround _ game playerId Station {  owner = Nothing  } =
  game { state = BuyOrNot playerId }
handleLandedOnGround _ game playerId Station { owner = Just owner  } =
  let
    gameAfterPay = pay playerId owner 25 game
    gameToContinueFrom = if playerId == owner then game else gameAfterPay
  in
    nextPlayer gameToContinueFrom playerId


nextPlayer game playerId =
  let
      nbPlayers = Data.Vector.length (players game)
      nextPlayerId = (playerId + 1) `mod` nbPlayers
      newState = DiceRoll nextPlayerId
      newGame = game {state = newState}

      newPlayer = players newGame ! nextPlayerId
    in
      addLog (Player.name newPlayer Prelude.++ " is next") newGame

pay :: Int -> Int -> Int -> Game -> Game
pay from to amount game =
  let
    playerFrom = Game.players game ! from
    playerTo = Game.players game ! to
    playerFromAfterPay = playerFrom { money = money playerFrom - amount}
    playerToAfterPay = playerTo { money = money playerTo + amount}
    newPlayers = Game.players game // [(to, playerToAfterPay), (from, playerFromAfterPay)]
  in
    addLog (Player.name playerFrom Prelude.++ " payed " Prelude.++ Player.name playerTo Prelude.++ " " Prelude.++ show amount Prelude.++ " rent") $ game {players = newPlayers}

dontBuy :: Int -> Game -> Game
dontBuy playerId game =
  let
    player = players game ! playerId
    playerName = Player.name $ player
    groundName = Ground.getGroundName $  grounds game ! (position player)
    msg = playerName Prelude.++ " decided not to buy " Prelude.++ groundName

    gameWithMsg = addLog msg game
  in
    nextPlayer gameWithMsg playerId

buyCash :: Int -> Game -> Game
buyCash playerId game =
  let
    player = players game ! playerId
    pos = Player.position player
    currentGround = (Game.grounds game) ! pos

    price = fromJust $ getCurrentValueOrInitial currentGround

    boughtGround = currentGround { owner = Just playerId, currentValue = Just price}
    newGrounds = (Game.grounds game) // [(pos, boughtGround)]

    playerAfterPaying = player { money = money player - price }
    newPlayers = (Game.players game) // [(playerId, playerAfterPaying)]

    economyAfterPaying = economy game + price

    gameAfterBuying = game { economy = economyAfterPaying, players = newPlayers, grounds = newGrounds}
    gameAfterLogging = addLog (Player.name player Prelude.++ " bought " Prelude.++ Ground.getGroundName boughtGround Prelude.++ " with cash for " Prelude.++ show price) gameAfterBuying
  in
    nextPlayer gameAfterLogging playerId

buyBorrow :: Int -> Game -> Game
buyBorrow playerId game =
  let
    player = players game ! playerId
    pos = Player.position player
    currentGround = (Game.grounds game) ! pos

    price = fromJust $ getCurrentValueOrInitial currentGround

    boughtGround = currentGround { owner = Just playerId, currentValue = Just price}
    newGrounds = (Game.grounds game) // [(pos, boughtGround)]

    playerAfterPaying = player { debt = debt player + price }
    newPlayers = (Game.players game) // [(playerId, playerAfterPaying)]

    economyAfterPaying = economy game + price

    gameAfterBuying = game { economy = economyAfterPaying, players = newPlayers, grounds = newGrounds}
    gameAfterLogging = addLog (Player.name player Prelude.++ " bought " Prelude.++ Ground.getGroundName boughtGround Prelude.++ " with a loan for " Prelude.++ show price) gameAfterBuying
  in
    nextPlayer gameAfterLogging playerId

addLog :: String -> Game -> Game
addLog msg game =
  let
    oldLogs = Game.logs game
    withExtra = msg : oldLogs
    newLogs = take 100 withExtra
  in
    game {logs = newLogs}