module InitGame where

import Game
import Ground
import Board
import Data.Vector(fromList)
import Data.IORef



initialGame :: IO (IORef Game)
initialGame =
  let
    game = (Game.initialGame InitGame.board 0.2 0.06666)
    (game2, _) = addPlayer "Dakke" game
    (game3, _) = addPlayer "Rebecca" game2
  in
    newIORef game3

main2 :: IO ()
main2 =
  let
    game1 = Game.initialGame InitGame.board 0.2 0.066666
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
          , rent = [2, 10, 30, 90, 160, 250]
          }
leuven = ownable
          { name = "Diestsestraat Leuven"
          , baseValue = 60
          , rent = [4, 20, 60, 180, 320, 450]
          }
brugge = ownable
          { name = "Steenstraat Brugge"
          , baseValue = 100
          , rent = [6, 30, 90, 270, 400, 550]
          }
oostende = ownable
          { name = "Kapellestraat Oostende"
          , baseValue = 120
          , rent = [8, 40, 100, 300, 450, 600]
          }
mechelen = ownable
          { name = "Place Du Monument Spa"
          , baseValue = 140
          , rent = [10, 50, 150, 450, 625, 750]
          }
verviers = ownable
          { name = "Place Verte Verviers"
          , baseValue = 160
          , rent = [12, 60, 180, 500, 700, 900]
          }
groenplaats = ownable
          { name = "Groenplaats Antwerpen"
          , baseValue = 200
          , rent = [16, 80, 220, 600, 800, 1000]
          }
tournai = ownable
          { name = "Rue Royale Tournai"
          , baseValue = 180
          , rent = [14, 70, 200, 550, 750, 950]
          }
mons = ownable
          { name = "Grand Place Mons"
          , baseValue = 240
          , rent = [20, 100, 300, 750, 925, 1100]
          }
kortrijk = ownable
          { name = "Lange Steenstraat Kortrijk"
          , baseValue = 220
          , rent = [18, 90, 250, 700, 875, 1050]
          }
namur = ownable
          { name = "Place De L'Ange Namur"
          , baseValue = 260
          , rent = [22, 110, 330, 800, 975, 1150]
          }
hoogstraat = ownable
          { name = "Hoogstraat Brussel"
          , baseValue = 280
          , rent = [24, 120, 360, 850, 1025, 1200]
          }
gent = ownable
          { name = "Veldstraat Gent"
          , baseValue = 300
          , rent = [26, 130, 390, 900, 1100, 1275]
          }
boulevard = ownable
          { name = "Boulevard D'Avroy Liege"
          , baseValue = 320
          , rent = [28, 150, 450, 1000, 1200, 1400]
          }
meir = ownable
          { name = "Meir Antwerpen"
          , baseValue = 350
          , rent = [35, 175, 500, 1100, 1300, 1500]
          }
nieuwstraat = ownable
          { name = "Nieuwstraat Brussel"
          , baseValue = 400
          , rent = [50, 200, 600, 1400, 1700, 2000]
          }

spa = brugge { name = "Place Du Monument Spa" }
arlon = mechelen { name = "Rue De Diekirch Arlon" }
knokke = tournai { name = "Lippenslaan Knokke" }
rueLiege = kortrijk { name = "Rue St. Leonard Liege" }
hasselt = namur { name = "Grote Markt Hasselt" }
charleroi = gent { name = "Boulevard Tirou Charleroi" }