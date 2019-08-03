module Logic where

import Game
import Types
import Ground
import Player

import Data.Maybe (fromJust)

addPlayer :: String -> Game -> Game
addPlayer name_ =
  addLog (name_ ++ " joined") . Game.addPlayer name_

startGame :: Game -> Game
startGame =
  addLog "Game started" . Game.startGame

rollDice :: Steps -> PlayerId -> Game -> Game
rollDice roll pId game =
  let
    (gameAfterMove, camePastStart) = movePlayer roll pId game

    oldGroundName = getPlayerGroundName pId game
    newGroundName = getPlayerGroundName pId gameAfterMove
    playerName = getPlayerName pId game

    msg = playerName ++
        " moved " ++ show roll ++
        " from " ++ oldGroundName ++
        " to " ++ newGroundName

    gameAfterMovingPlayer = addLog msg gameAfterMove
    gameAfterStartMoney = if camePastStart
          then applyStartMoney pId gameAfterMovingPlayer
          else gameAfterMovingPlayer

    gameAfterHandling = handleLanding roll pId gameAfterStartMoney
  in
    gameAfterHandling


applyStartMoney :: PlayerId -> Game -> Game
applyStartMoney pId game =
  let
    amount = calculateStartMoney pId game
    gameAfterUpdates = fromEconomyToPlayer amount pId game

    msg = getPlayerName pId game ++ " received " ++ show amount ++ " start money"
  in
    addLog msg gameAfterUpdates

handleLanding :: Steps -> PlayerId -> Game -> Game
handleLanding roll pId game =
  let
    currentGround = getPlayerGround pId game
  in
    handleLandedOnGround roll pId currentGround game


-- todo fix param order
handleLandedOnGround :: Steps -> PlayerId -> Ground -> Game -> Game
handleLandedOnGround _ pId Start game =
  Logic.nextPlayer pId game

handleLandedOnGround _ pId FreeParking game =
  Logic.nextPlayer pId game

handleLandedOnGround _ pId ExtraTax {} game =
  Logic.nextPlayer pId game --todo

handleLandedOnGround roll pId ground game =
  let
    maybeOwner = getOwner ground
  in
    case maybeOwner of
      Nothing -> game { state = BuyOrNot pId}
      Just owner_ ->
        let
          amount = Ground.amountToPay roll ground
          gameAfterPay = pay amount pId owner_ game
          gameToContinueFrom = if owner_ == pId  then game else gameAfterPay
        in
          Logic.nextPlayer pId gameToContinueFrom



nextPlayer :: PlayerId -> Game -> Game
nextPlayer pId game =
  let
      newGame = Game.nextPlayer pId game
      newPlayer = currentPlayer game
    in
      addLog (getPlayerName newPlayer game ++ " is next") newGame

pay :: Amount -> PlayerId -> PlayerId -> Game -> Game
pay amount from to game =
  let
    gameAfterPaying = updatePlayer (removeMoney amount) from . updatePlayer (addMoney amount) to $ game
    msg = getPlayerName from game ++ " payed " ++ getPlayerName to game ++ " " ++ show amount ++ " rent"
  in
    addLog msg gameAfterPaying


dontBuy :: PlayerId -> Game -> Game
dontBuy pId game =
  let
    playerName = getPlayerName pId game
    groundName = getPlayerGroundName pId game
    msg = playerName ++ " decided not to buy " ++ groundName

    gameWithMsg = addLog msg game
  in
    Logic.nextPlayer pId gameWithMsg


buyCash :: PlayerId -> Game -> Game
buyCash pId game =
  let
    currentGround = getPlayerGround pId game
    price = fromJust $ getCurrentValueOrInitial currentGround

    chOwner = updateGround (changeOwner pId) (getPlayerPosition pId game)
    buy = fromPlayerToEconomy price pId

    gameAfterBuying = chOwner . buy $ game
    msg = getPlayerName pId game ++ " bought " ++ Ground.getGroundName currentGround ++ " for " ++ show price
    gameAfterLogging = addLog msg gameAfterBuying
  in
    Logic.nextPlayer pId gameAfterLogging

buyBorrow :: PlayerId -> Game -> Game
buyBorrow pId game =
  let
    currentGround = getPlayerGround pId game
    price = fromJust $ getCurrentValueOrInitial currentGround

    gameAfterBorrow = updatePlayer (borrow price) pId game

    msg = getPlayerName pId game ++ " borrowed " ++ show price
    gameAfterLogging = addLog msg gameAfterBorrow
  in
    buyCash pId gameAfterLogging

payBack :: Amount -> PlayerId -> Game -> Game
payBack amount pId game =
  let
    gameAfterUpdate = updatePlayer (Player.payBack amount) pId game
    msg = getPlayerName pId game ++ " paid back " ++ show amount
  in
    addLog msg gameAfterUpdate