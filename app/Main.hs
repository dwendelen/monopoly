module Main where

import Lib
import Game

main :: IO ()
main = someFunc


board :: Board
board = Board
    { grounds =
        [ FreeParking

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
        , ExtraTax { name = "Extra Belasting", owner = Nothing, currentValue = Nothing }
        , nieuwstraat
        ]
    }

ownable = OwnableGround { currentValue = Nothing, owner = Nothing }

dinant = ownable
          { name = "Rue Grande Dinant"
          , baseValue = 60
          , price = 60
          , rent :: [2, 10, 30 ,90, 160, 250]
          }
leuven = ownable
          { name = "Diestsestraat Leuven"
          , baseValue = 60
          , rent :: [4, 20, 60 ,180, 320, 450]
          }
brugge = dinant
spa = leuven
oostende = leuven
arlon = dinant
mechelen = oostende
verviers = mechelen
knokke = leuven
tournai = dinant
groenplaats = knokke
rueLiege = groenplaats
kortrijk = leuven
mons = dinant
hasselt = dinant
namur = leuven
hoogstraat = leuven
gent = verviers
charleroi = kortrijk
boulevard = tournai
meir = oostende
nieuwstraat = spa