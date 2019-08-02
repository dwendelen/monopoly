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


main :: IO ()
main = do
    newGame <- InitGame.initialGame
    -- Get the static subsite, as well as the settings it is based on
    static@(Static settings) <- static "static"
    warpDebug 3000 $ App { getStatic = static, game = newGame }

