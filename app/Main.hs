{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings, TypeFamilies, MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import Yesod
import Yesod.Static
import Data.Text (Text)

import Data.IORef
import Game (Game, buyCash, buyBorrow, payBack) --todo reduce
import Logic
import InitGame
import View

staticFiles "static"

data App = App
    { getStatic :: Static
    , game :: IORef Game
    }

mkYesod "App" [parseRoutes|
/static StaticR Static getStatic
/state StateR GET
/game/startGame StartGameR POST
/game/roll RollR POST
/game/dont-buy DontBuyR POST
/game/buy-cash BuyCashR POST
/game/buy-borrow BuyBorrowR POST
/game/pay-back-debt PayBackDebtR POST
|]

instance Yesod App

data Person = Person
    { name :: Text
    , age  :: Int
    }

instance ToJSON Person where
    toJSON Person {..} = object
        [ "name" .= name
        , "age"  .= age
        ]

getStateR :: Handler  Value
getStateR = do
  gameRef <- fmap game getYesod
  game_ <- liftIO . readIORef $ gameRef
  returnJson (mapGame game_)

postStartGameR :: Handler Value
postStartGameR = doAction Logic.startGame

postRollR :: Handler Value
postRollR = do
  rollRTO <- requireCheckJsonBody :: Handler RollRTO
  doAction (rollDice (roll rollRTO) (View.rollPlayer rollRTO))

postDontBuyR :: Handler Value
postDontBuyR = do
  playerOnly <- requireCheckJsonBody :: Handler PlayerOnly
  doAction (dontBuy (View.player playerOnly))

postBuyCashR :: Handler Value
postBuyCashR = do
    playerOnly <- requireCheckJsonBody :: Handler PlayerOnly
    doAction (buyCash (View.player playerOnly))

postBuyBorrowR :: Handler Value
postBuyBorrowR = do
    playerOnly <- requireCheckJsonBody :: Handler PlayerOnly
    doAction (buyBorrow (View.player playerOnly))

postPayBackDebtR :: Handler Value
postPayBackDebtR = do
  payBackRTO <- requireCheckJsonBody :: Handler PayBackRTO
  doAction (payBack (payBackPlayer payBackRTO) (payBackAmount payBackRTO))

doAction :: (Game -> Game) -> Handler Value
doAction action = do
  gameRef <- fmap game getYesod
  liftIO $ do
    game1 <- readIORef gameRef
    let game2 = action game1
    writeIORef gameRef game2

  returnJson (object [])

--main = main2

main :: IO ()
main = do
    newGame <- InitGame.initialGame
    static_ <- static "static"
    warp 3000 $ App { getStatic = static_, game = newGame }

