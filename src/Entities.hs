--{-# LANGUAGE TemplateHaskell #-}


module Entities where
--import Control.Lens
--import Producers

data EName = Scv
           | SupplyDepot
           | CommandCenter
           | CommandCenter'
           | Barracks
           | Marine
           | Nil
             deriving (Show, Eq, Enum, Read)


data BaseStats = Stats
               { name'      :: EName
               , mins'      :: Double
               , gas'       :: Double
               , buildTime' :: Int
               , supply'    :: Int
               , builtBy'   :: [EName]
               } deriving (Show, Eq)


-- Producer Attributes
--data PAttr = PAttr
--             { building' :: EName
--             , timeLeft' :: Int
--             } deriving (Show, Eq)

-- Command Attributes
--data CAttr = CAttr
--             { workers :: [Entity] } deriving (Show, Eq)


data Entity = Entity   { baseStats :: BaseStats
                       }
            | Producer { baseStats :: BaseStats
                       , building  :: EName
                       , timeLeft  :: Int
                       }
            | Command  { baseStats :: BaseStats
                       , building  :: EName
                       , timeLeft  :: Int
                       , workers   :: [Entity]
                       }
            | NoEntity
              deriving (Show, Eq)

buildTime :: Entity -> Int
buildTime e = buildTime'(baseStats e)

builtBy :: Entity -> [EName]
builtBy e = builtBy'(baseStats e)

name :: Entity -> EName
name e = name'(baseStats e)

--scv = create Scv

create :: EName -> Entity
create Scv            = Producer (Stats Scv            50 0  17 1 [CommandCenter]) Nil 0
create SupplyDepot    = Entity   (Stats SupplyDepot   100 0  30 0 [Scv])
create CommandCenter  = Command  (Stats CommandCenter 400 0 100 0 [Scv])           Nil 0 []
create CommandCenter' = Command  (Stats CommandCenter 400 0 100 0 [Scv])           Nil 0 [s,s,s,s,s,s] where s = create Scv
create Barracks       = Producer (Stats Barracks      150 0  65 0 [Scv])           Nil 0
create Marine         = Entity   (Stats Marine         50 0  25 1 [Barracks])
create Nil            = NoEntity


--build :: Entity -> EName -> Int -> Entity
--build producer = Producer (baseStats producer)

build :: Entity -> EName -> Int -> Entity
build e n t = e { building = n, timeLeft = t }


toEntityList :: [String] -> [EName]
toEntityList (x:xs) = read x : toEntityList xs
toEntityList  _     = []

toStrList :: [EName] -> [String]
toStrList (x:xs) = show x : toStrList xs
toStrList  _     = []


-- TODO
-- Appends self to new list along with any newly built entities.
--updateEntity :: Entity -> [Entity] -> [Entity]
--updateEntity (Producer _ _ _)            
--updateEntity (Producer stats Nil _)       es   = Producer stats Nil 0 : es
--updateEntity (Producer stats e remaining) es   = updateProducer (Producer(stats) e remaining)
  --                            | remaining == 0 = Producer stats Nil 0 : create e : es
   --                           | otherwise      = Producer stats e (remaining -1) : es
--updateEntity e                            es   = e                    : es


--removeItem :: Eq a => a -> [a] -> [a]
--removeItem _ []                 = []
--removeItem x (y:ys) | x == y    = removeItem x ys
--                    | otherwise = y : removeItem x ys


scvProvidedSupply :: Int
scvProvidedSupply = 8
                       


--TODO
-- Create new AllEntities list from current.
--getNewEntities :: [Entity] -> [Entity]
--getNewEntities = foldr updateEntity []








