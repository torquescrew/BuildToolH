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

instance CommonGetters GameState where
   mins   = gsMins
   gas    = gsGas
   supply = gsSupply
   

canAfford :: Entity -> GameState -> Bool
canAfford e gs
        |  mins gs >= mins e
        && gas  gs >= gas  e
        && supplyFree gs >= supply e = True
canAfford _ _                        = False


payFor :: Entity -> GameState -> GameState
payFor e gs = gs { gsMins =  mins gs - mins e
                 , gsGas = gas gs - gas e
                 , gsSupply = supply gs + supply e }
                 


hasBuilder :: Entity -> GameState -> Bool
hasBuilder e gs = hasBuilder' (builtBy e) (entities gs)

hasBuilder' :: [EName] -> [Entity] -> Bool
--hasBuilder' (x:xs) ae = any ((==x) . name) ae || hasBuilder' xs ae
hasBuilder' (x:xs) ae = any (canBuild x) ae || hasBuilder' xs ae
                        where canBuild n e = n == name e && building e == Nil
hasBuilder' _      _  = False




