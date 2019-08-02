module InitGame where

import Game
import Ground
import Board
import Data.Vector(fromList)
import Data.IORef



initialGame :: IO (IORef Game)
initialGame = newIORef (Game.initialGame InitGame.board)

main2 :: IO ()
main2 =
  let
    game1 = Game.initialGame InitGame.board
    (game2, msgs2) = addPlayer "Player 1" game1
    (game3, msgs3) = addPlayer "Player 2" game2
    (game4, msgs4) = addPlayer "Player 3" game3
    (game5, msgs5) = startGame game4
    (game6, msgs6) = rollDice 0 5 game5
    (game7, msgs7) = rollDice 1 9 game6
    (game8, msgs8) = rollDice 2 10 game7
    (game9, msgs9) = rollDice 0 39 game8
  in
    do
      mapM_ putStrLn msgs2
      mapM_ putStrLn msgs3
      mapM_ putStrLn msgs4
      mapM_ putStrLn msgs5
      mapM_ putStrLn msgs6
      mapM_ putStrLn msgs7
      mapM_ putStrLn msgs8
      mapM_ putStrLn msgs9

board :: Board
board = Board
    { grounds =
        fromList [ FreeParking

        , dinant
        , FreeParking
        , leuven
        , ExtraTax { name = "Inkomsten Belasting", tax = 200 }
        , Station { name = "Noord Station", owner = Nothing, currentValue = Nothing }
        , brugge
        , FreeParking
        , spa
        , oostende

        , FreeParking

        , arlon
        , Utility { name = "Elektriciteitscentrale", owner = Nothing, currentValue = Nothing }
        , mechelen
        , verviers
        , Station { name = "Centraal Station", owner = Nothing, currentValue = Nothing }
        , knokke
        , FreeParking
        , tournai
        , groenplaats

        , FreeParking

        , rueLiege
        , FreeParking
        , kortrijk
        , mons
        , Station { name = "Buurt Spoorwegen", owner = Nothing, currentValue = Nothing }
        , hasselt
        , namur
        , Utility { name = "Watermaatschappij", owner = Nothing, currentValue = Nothing }
        , hoogstraat

        , FreeParking

        , gent
        , charleroi
        , FreeParking
        , boulevard
        , Station { name = "Zuid Station", owner = Nothing, currentValue = Nothing }
        , FreeParking
        , meir
        , ExtraTax { name = "Extra Belasting", tax = 100 }
        , nieuwstraat
        ]
    }

ownable = OwnableGround
    { currentValue = Nothing
    , owner = Nothing
    , name = ""
    , baseValue = 0
    , rent = []
    }

dinant = ownable
          { name = "Rue Grande Dinant"
          , baseValue = 60
          , rent = [2, 10, 30 ,90, 160, 250]
          }
leuven = ownable
          { name = "Diestsestraat Leuven"
          , baseValue = 60
          , rent = [4, 20, 60 ,180, 320, 450]
          }
brugge = dinant { name = "Steenstraat Brugge" }
spa = leuven { name = "Place Du Monument Spa" }
oostende = leuven { name = "Kapellestraat Oostende" }
arlon = dinant { name = "Rue De Diekirch Arlon" }
mechelen = oostende { name = "Bruul Mechelen" }
verviers = mechelen { name = "Place Verte Verviers" }
knokke = leuven { name = "Lippenslaan Knokke" }
tournai = dinant { name = "Rue Royale Tournai" }
groenplaats = knokke { name = "Groenplaats Antwerpen" }
rueLiege = groenplaats { name = "Rue St. Leonard Liege" }
kortrijk = leuven { name = "Lange Steenstraat Kortrijk" }
mons = dinant { name = "Grand Place Mons" }
hasselt = dinant { name = "Grote Markt Hasselt" }
namur = leuven { name = "Place De L'Ange Namur" }
hoogstraat = leuven { name = "Hoogstraat Brussel" }
gent = verviers { name = "Veldstraat Gent" }
charleroi = kortrijk { name = "Boulevard Tirou Charleroi" }
boulevard = tournai { name = "Boulevard D'Avroy Liege" }
meir = oostende { name = "Meir Antwerpen" }
nieuwstraat = spa { name = "Nieuwstraat Brussel" }