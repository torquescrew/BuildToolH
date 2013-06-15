module Entities where

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


create :: EName -> Entity
create Scv            = Producer (Stats Scv            50 0  17 1 [CommandCenter]) Nil 0
create SupplyDepot    = Entity   (Stats SupplyDepot   100 0  30 0 [Scv])
create CommandCenter  = Command  (Stats CommandCenter 400 0 100 0 [Scv])           Nil 0 []
create CommandCenter' = Command  (Stats CommandCenter 400 0 100 0 [Scv])           Nil 0 [s,s,s,s,s,s] where s = create Scv
create Barracks       = Producer (Stats Barracks      150 0  65 0 [Scv])           Nil 0
create Marine         = Entity   (Stats Marine         50 0  25 1 [Barracks])
create Nil            = NoEntity


build :: Entity -> EName -> Int -> Entity
build e n t = e { building = n, timeLeft = t }


toEntityList :: [String] -> [EName]
toEntityList (x:xs) = read x : toEntityList xs
toEntityList  _     = []

toStrList :: [EName] -> [String]
toStrList (x:xs) = show x : toStrList xs
toStrList  _     = []


scvProvidedSupply :: Int
scvProvidedSupply = 8
                       





