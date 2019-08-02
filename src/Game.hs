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
    nbPlaces = Data.Vector.length (grounds $ game)

    player = players game ! playerId
    oldPosition = position player
    newPosition = (oldPosition + roll) `mod` nbPlaces
    camePastStart = oldPosition < newPosition

    newPlayer = player { position = newPosition }
    newPlayers = players game Data.Vector.// [(playerId, newPlayer)]

    gameAfterMovingPlayer = game { players = newPlayers }

    (gameAfterHandling, msgs) = handleLanding roll gameAfterMovingPlayer playerId camePastStart


    msg = Player.name player Prelude.++
        " moved " Prelude.++ show roll Prelude.++
        " from " Prelude.++ show oldPosition Prelude.++
        " to " Prelude.++ show newPosition
  in
    (gameAfterHandling, [msg] Prelude.++ msgs)

handleLanding :: Int -> Game -> Int -> Bool -> (Game, [String])
handleLanding roll game playerId camePastStart =
  let
    player = players game ! playerId
    pos = Player.position player

    currentGround = (Game.grounds game) ! pos
  in
    handleLandedOnGround roll game playerId currentGround

handleLandedOnGround :: Int -> Game -> Int -> Ground -> (Game, [String])
handleLandedOnGround _ game playerId FreeParking =
  nextPlayer game playerId
handleLandedOnGround _ game playerId ExtraTax {} =
  nextPlayer game playerId --todo

handleLandedOnGround _ game playerId OwnableGround {  owner = Nothing  } =
  (game { state = BuyOrNot playerId }, [])
handleLandedOnGround _ game playerId OwnableGround {  rent = rent , owner = Just owner  } =
  let
    gameAfterPay = pay playerId owner (head rent) game
    gameToContinueFrom = if playerId == owner then game else gameAfterPay
  in
    nextPlayer gameToContinueFrom playerId

handleLandedOnGround _ game playerId Utility {  owner = Nothing  } =
  (game { state = BuyOrNot playerId }, [])
handleLandedOnGround roll game playerId Utility { owner = Just owner  } =
  let
    gameAfterPay = pay playerId owner (4 * roll) game
    gameToContinueFrom = if playerId == owner then game else gameAfterPay
  in
    nextPlayer gameToContinueFrom playerId

handleLandedOnGround _ game playerId Station {  owner = Nothing  } =
  (game { state = BuyOrNot playerId }, [])
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
    in
      (newGame, [])

pay :: Int -> Int -> Int -> Game -> Game
pay from to amount game =
  let
    playerFrom = Game.players game ! from
    playerTo = Game.players game ! to
    playerFromAfterPay = playerFrom { money = money playerFrom - amount}
    playerToAfterPay = playerTo { money = money playerTo + amount}
    newPlayers = Game.players game // [(to, playerToAfterPay), (from, playerFromAfterPay)]
  in
    game {players = newPlayers}

buyCash :: Int -> Game -> (Game, [String])
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
  in
    nextPlayer gameAfterBuying playerId

buyBorrow = buyCash