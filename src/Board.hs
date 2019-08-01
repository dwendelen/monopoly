module Board where

import Ground
import Data.Vector

data Board = Board
  { grounds :: Vector Ground
  }