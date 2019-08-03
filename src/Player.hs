module Player where

import Types

data Player = Player
  { name :: String
  , debt :: Amount
  , money :: Amount
  , position :: Position
  }

move :: Steps -> Int -> Player -> Player
move amount modulo player =
  let
    pos = position player
    newPos = (pos + amount) `mod` modulo
  in
    player { position = newPos }

addMoney :: Amount -> Player -> Player
addMoney amount player =
  player { money = money player + amount }

removeMoney :: Amount -> Player -> Player
removeMoney amount = addMoney (-amount)

borrow :: Amount -> Player -> Player
borrow amount player =
  player { money = money player + amount, debt = debt player + amount }

payBack :: Amount -> Player -> Player
payBack amount =
  borrow (-amount)
