{-# LANGUAGE TemplateHaskell #-}

module Entities2 where
import Control.Lens
--import GameState2

data EName = Scv
           | SupplyDepot
           | CommandCenter
           | Barracks
           | Marine
           | Nil
             deriving (Show, Eq, Enum, Read)
             
--data BaseStats = Stats
--               { _name      :: EName
--               , _mins      :: Double
--               , _gas       :: Double
--               , _buildTime :: Int
--               , _supply    :: Int
--               , _builtBy   :: [EName]
--               } deriving (Show, Eq)
               
data Entity = Entity
               { _eName      :: EName
               , _eMins      :: Double
               , _eGas       :: Double
               , _eBuildTime :: Int
               , _eSupply    :: Int
               , _eBuiltBy   :: [EName]
               }
            | Producer
               { _eName      :: EName
               , _eMins      :: Double
               , _eGas       :: Double
               , _eBuildTime :: Int
               , _eSupply    :: Int
               , _eBuiltBy   :: [EName]
               , _eBuilding  :: EName
               , _eTimeLeft  :: Int
               }
            | Command
               { _eName      :: EName
               , _eMins      :: Double
               , _eGas       :: Double
               , _eBuildTime :: Int
               , _eSupply    :: Int
               , _eBuiltBy   :: [EName]
               , _eBuilding  :: EName
               , _eTimeLeft  :: Int
               , _eWorkers   :: [Entity]
               }
--            | NoEntity
--               { _eName      :: EName }
              deriving (Show, Eq)
              
--makeLenses ''BaseStats
makeLenses ''Entity




--makeClassyFor "Entity" "BaseStats" [] ''Entity

--gs = GameState 50.0

--gs' = gs^..mins

marine = Entity Marine 50 0 25 1 [Barracks]
--stats = Stats Marine         50 0  25 1 [Barracks]

s = marine^.eMins

m2 = marine & eMins .~ 5
m3 = marine & eMins +~ 3


scv = Producer Scv 50 0 17 1 [CommandCenter] Nil 0
w = scv ^. eWorkers
--(->) :: Entity -> BaseStats
--quickStats e = e^.baseStats


--add :: Entity 