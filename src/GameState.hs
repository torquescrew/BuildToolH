
module GameState where
--import CommonInterface
import Entities


data GameState = GameState
        { gsMins      :: Double
        , gsGas       :: Double
        , gsSupply    :: Int
        , gsSupplyMax :: Int
        , gsTime      :: Int
        , commands    :: [Entity]
        , producers   :: [Entity]
        } deriving Show

supplyFree :: GameState -> Int
supplyFree gs = gsSupplyMax gs - gsSupply gs


-- Collects all entities for querying
entities :: GameState -> [Entity]
entities gs = commands gs ++ producers gs


-- Return new GameState where the given entity is being built. Checks all entities when trying to build.
startBuilding :: Entity -> GameState -> GameState
startBuilding e gs = gs { commands  = startBuilding' e (commands  gs)
                        , producers = startBuilding' e (producers gs)
                        }


-- Return new entity list where the given entity is being built.
startBuilding' :: Entity -> [Entity] -> [Entity]
startBuilding' _ []                      = []
startBuilding' e (p:ae) 
               | name p `elem` builtBy e = build p (name e) (buildTime e) : ae
               | otherwise               = p : startBuilding' e ae
                        

incrementTime :: GameState -> GameState
incrementTime gs = gs { gsTime = gsTime gs + 1 }



