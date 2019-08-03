module Logic where

import Game
import Types
import Ground
import Player

addPlayer :: String -> Game -> Game
addPlayer name_ =
  addLog (name_ Prelude.++ " joined") . Game.addPlayer name_

startGame :: Game -> Game
startGame =
  addLog "Game started" . Game.startGame

-- todo swap pid
rollDice :: PlayerId -> Steps -> Game -> Game
rollDice pId roll game =
  let
    (gameAfterMove, camePastStart) = movePlayer roll pId game

    oldGroundName = getPlayerGroundName pId game
    newGroundName = getPlayerGroundName pId gameAfterMove
    playerName = getPlayerName pId game

    msg = playerName Prelude.++
        " moved " Prelude.++ show roll Prelude.++
        " from " Prelude.++ oldGroundName Prelude.++
        " to " Prelude.++ newGroundName

    gameAfterMovingPlayer = addLog msg gameAfterMove
    gameAfterStartMoney = if camePastStart
          then applyStartMoney gameAfterMovingPlayer pId
          else gameAfterMovingPlayer

    gameAfterHandling = handleLanding roll pId gameAfterStartMoney
  in
    gameAfterHandling


-- todo swap pid
applyStartMoney :: Game -> PlayerId -> Game
applyStartMoney game pId =
  let
    amount = calculateStartMoney pId game
    gameAfterUpdates = fromEconomyToPlayer amount pId game

    msg = getPlayerName pId game Prelude.++ " received " Prelude.++ show amount Prelude.++ " start money"
  in
    addLog msg gameAfterUpdates

handleLanding :: Steps -> PlayerId -> Game -> Game
handleLanding roll pId game =
  let
    currentGround = getPlayerGround pId game
  in
    handleLandedOnGround roll game pId currentGround


-- todo fix param order
handleLandedOnGround :: Steps -> Game -> PlayerId -> Ground -> Game
handleLandedOnGround _ game playerId Start =
  Logic.nextPlayer game playerId
handleLandedOnGround _ game playerId FreeParking =
  Logic.nextPlayer game playerId
handleLandedOnGround _ game playerId ExtraTax {} =
  Logic.nextPlayer game playerId --todo

handleLandedOnGround roll game pId ground =
  let
    maybeOwner = getOwner ground
  in
    case maybeOwner of
      Nothing -> game { state = BuyOrNot pId}
      Just owner_ ->
        let
          amount = Ground.amountToPay roll ground
          gameAfterPay = pay pId owner_ amount game
          gameToContinueFrom = if owner_ == pId  then game else gameAfterPay
        in
          Logic.nextPlayer gameToContinueFrom pId



-- todo order
nextPlayer :: Game -> PlayerId -> Game
nextPlayer game playerId =
  let
      newGame = Game.nextPlayer game playerId
      newPlayer = currentPlayer game
    in
      addLog (getPlayerName newPlayer game Prelude.++ " is next") newGame

-- todo order pid and amount
pay :: PlayerId -> PlayerId -> Amount -> Game -> Game
pay from to amount game =
  let
    gameAfterPaying = updatePlayer from (removeMoney amount) . updatePlayer to (addMoney amount) $ game
    msg = getPlayerName from game Prelude.++ " payed " Prelude.++ getPlayerName to game Prelude.++ " " Prelude.++ show amount Prelude.++ " rent"
  in
    addLog msg gameAfterPaying


dontBuy :: PlayerId -> Game -> Game
dontBuy playerId game =
  let
    playerName = getPlayerName playerId game
    groundName = getPlayerGroundName playerId game
    msg = playerName Prelude.++ " decided not to buy " Prelude.++ groundName

    gameWithMsg = addLog msg game
  in
    Logic.nextPlayer gameWithMsg playerId