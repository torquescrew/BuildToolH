{-# LANGUAGE TemplateHaskell #-}

module GameState2 where
import Control.Lens
import Entities2

data GameState = GameState
        { _mins      :: Double
        , _gas       :: Double
        , _supply    :: Int
        , _supplyMax :: Int
        , _time      :: Int
        , _entities  :: [Entity]
        } deriving Show

makeLenses ''GameState

supplyFree :: GameState -> Int
supplyFree gs = gs^.supplyMax - gs^.supply

canAfford :: Entity -> GameState -> Bool
canAfford e gs |  gs^.mins >= e^.eMins
               && gs^.gas  >= e^.eGas
               && supplyFree gs >= e^.eSupply = True
canAfford _ _                                 = False


--payFor :: Entity -> GameState -> GameState
--payFor e gs 