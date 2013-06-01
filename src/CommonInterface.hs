module CommonInterface where

import Entities   
import GameState  

class CommonGetters a where
   mins   :: a -> Double
   gas    :: a -> Double
   supply :: a -> Int
   
instance CommonGetters Entity where
   mins   e = Entities.mins'   (baseStats e)
   gas    e = Entities.gas'    (baseStats e)
   supply e = Entities.supply' (baseStats e)

instance CommonGetters GameStats where
   mins   = GameState.mins'
   gas    = GameState.gas'
   supply = GameState.supply'
   

canAfford :: Entity -> GameStats -> Bool
canAfford e gs
        |  mins gs >= mins e
        && gas  gs >= gas  e
        && supplyFree gs >= supply e = True
canAfford _ _                        = False


payFor :: Entity -> GameStats -> GameStats
payFor e gs = GStats (mins gs - mins e) (gas gs - gas e) (supply gs + supply e) (supplyMax gs) (time gs)
--payFor e gs = gs { mins' = 5 }


hasBuilder :: Entity -> [Entity] -> Bool
hasBuilder e = hasBuilder' (builtBy e)

hasBuilder' :: [EName] -> [Entity] -> Bool
--hasBuilder' (x:xs) ae = any ((==x) . name) ae || hasBuilder' xs ae
hasBuilder' (x:xs) ae = any (canBuild x) ae || hasBuilder' xs ae
                        where canBuild n e = n == name e && building e == Nil
hasBuilder' _      _  = False




