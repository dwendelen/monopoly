{-# LANGUAGE QuasiQuotes, TemplateHaskell, OverloadedStrings, TypeFamilies, MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import Yesod
import Yesod.Static
import Data.Text (Text)

import Data.IORef
import Game
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
postStartGameR = doAction startGame

postRollR :: Handler Value
postRollR = do
  rollRTO <- requireJsonBody :: Handler RollRTO
  doAction (rollDice (View.rollPlayer rollRTO) (roll rollRTO))

postDontBuyR :: Handler Value
postDontBuyR = do
  playerOnly <- requireJsonBody :: Handler PlayerOnly
  doAction (\g -> nextPlayer g (View.player playerOnly))

postBuyCashR :: Handler Value
postBuyCashR = do
    playerOnly <- requireJsonBody :: Handler PlayerOnly
    doAction (buyCash (View.player playerOnly))

postBuyBorrowR :: Handler Value
postBuyBorrowR = do
    playerOnly <- requireJsonBody :: Handler PlayerOnly
    doAction (buyBorrow (View.player playerOnly))

doAction :: (Game -> (Game, [String])) -> Handler Value
doAction action = do
  gameRef <- fmap game getYesod
  liftIO $ do
    game1 <- readIORef gameRef
    let (game2, _) = action game1
    writeIORef gameRef game2

  returnJson (object [])

--main = main2

main :: IO ()
main = do
    newGame <- InitGame.initialGame
    -- Get the static subsite, as well as the settings it is based on
    static@(Static settings) <- static "static"
    warpDebug 3000 $ App { getStatic = static, game = newGame }

