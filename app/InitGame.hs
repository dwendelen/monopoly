module InitGame where

import Game
import Ground
import Data.Vector(Vector, fromList)
import Data.IORef



initialGame :: IO (IORef Game)
initialGame =
  let
    game = Game.initialGame InitGame.board 0.2 0.06666
    game2 = addPlayer "Dakke" game
    game3 = addPlayer "Rebecca" game2
    game4 = addPlayer "Mick" game3
  in
    newIORef game4

ownable :: Ground
ownable = OwnableGround
    { owner = Nothing
    , name = ""
    , baseValue = 0
    , rent = []
    , color = ""
    }

board :: Vector Ground
board =
      fromList [
        Start

        , dinant
        , FreeParking
        , leuven
        , ExtraTax { name = "Inkomsten Belasting", tax = 200 }
        , Station { name = "Noord Station", owner = Nothing }
        , brugge
        , FreeParking
        , spa
        , oostende

        , FreeParking

        , arlon
        , Utility { name = "Elektriciteitscentrale", owner = Nothing }
        , mechelen
        , verviers
        , Station { name = "Centraal Station", owner = Nothing }
        , knokke
        , FreeParking
        , tournai
        , groenplaats

        , FreeParking

        , rueLiege
        , FreeParking
        , kortrijk
        , mons
        , Station { name = "Buurt Spoorwegen", owner = Nothing }
        , hasselt
        , namur
        , Utility { name = "Watermaatschappij", owner = Nothing }
        , hoogstraat

        , FreeParking

        , gent
        , charleroi
        , FreeParking
        , boulevard
        , Station { name = "Zuid Station", owner = Nothing }
        , FreeParking
        , meir
        , ExtraTax { name = "Extra Belasting", tax = 100 }
        , nieuwstraat
        ]
      where
        dinant = ownable
                  { name = "Rue Grande Dinant"
                  , baseValue = 60
                  , rent = [2, 10, 30, 90, 160, 250]
                  , color = "brown"
                  }
        leuven = ownable
                  { name = "Diestsestraat Leuven"
                  , baseValue = 60
                  , rent = [4, 20, 60, 180, 320, 450]
                  , color = "brown"
                  }
        brugge = ownable
                  { name = "Steenstraat Brugge"
                  , baseValue = 100
                  , rent = [6, 30, 90, 270, 400, 550]
                  , color = "lightBlue"
                  }
        oostende = ownable
                  { name = "Kapellestraat Oostende"
                  , baseValue = 120
                  , rent = [8, 40, 100, 300, 450, 600]
                  , color = "lightBlue"
                  }
        mechelen = ownable
                  { name = "Place Du Monument Spa"
                  , baseValue = 140
                  , rent = [10, 50, 150, 450, 625, 750]
                  , color = "pink"
                  }
        verviers = ownable
                  { name = "Place Verte Verviers"
                  , baseValue = 160
                  , rent = [12, 60, 180, 500, 700, 900]
                  , color = "pink"
                  }
        groenplaats = ownable
                  { name = "Groenplaats Antwerpen"
                  , baseValue = 200
                  , rent = [16, 80, 220, 600, 800, 1000]
                  , color = "orange"
                  }
        tournai = ownable
                  { name = "Rue Royale Tournai"
                  , baseValue = 180
                  , rent = [14, 70, 200, 550, 750, 950]
                  , color = "orange"
                  }
        mons = ownable
                  { name = "Grand Place Mons"
                  , baseValue = 240
                  , rent = [20, 100, 300, 750, 925, 1100]
                  , color = "red"
                  }
        kortrijk = ownable
                  { name = "Lange Steenstraat Kortrijk"
                  , baseValue = 220
                  , rent = [18, 90, 250, 700, 875, 1050]
                  , color = "red"
                  }
        namur = ownable
                  { name = "Place De L'Ange Namur"
                  , baseValue = 260
                  , rent = [22, 110, 330, 800, 975, 1150]
                  , color = "yellow"
                  }
        hoogstraat = ownable
                  { name = "Hoogstraat Brussel"
                  , baseValue = 280
                  , rent = [24, 120, 360, 850, 1025, 1200]
                  , color = "yellow"
                  }
        gent = ownable
                  { name = "Veldstraat Gent"
                  , baseValue = 300
                  , rent = [26, 130, 390, 900, 1100, 1275]
                  , color = "green"
                  }
        boulevard = ownable
                  { name = "Boulevard D'Avroy Liege"
                  , baseValue = 320
                  , rent = [28, 150, 450, 1000, 1200, 1400]
                  , color = "green"
                  }
        meir = ownable
                  { name = "Meir Antwerpen"
                  , baseValue = 350
                  , rent = [35, 175, 500, 1100, 1300, 1500]
                  , color = "darkBlue"
                  }
        nieuwstraat = ownable
                  { name = "Nieuwstraat Brussel"
                  , baseValue = 400
                  , rent = [50, 200, 600, 1400, 1700, 2000]
                  , color = "darkBlue"
                  }
        spa = brugge { name = "Place Du Monument Spa" }
        arlon = mechelen { name = "Rue De Diekirch Arlon" }
        knokke = tournai { name = "Lippenslaan Knokke" }
        rueLiege = kortrijk { name = "Rue St. Leonard Liege" }
        hasselt = namur { name = "Grote Markt Hasselt" }
        charleroi = gent { name = "Boulevard Tirou Charleroi" }