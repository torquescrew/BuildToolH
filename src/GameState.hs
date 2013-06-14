
module GameState where
--import CommonInterface
import Entities
import Producers
import CommandBuilding


data GameState = GameState
        { gsMins      :: Double
        , gsGas       :: Double
        , gsSupply    :: Int
        , gsSupplyMax :: Int
        , gsTime      :: Int
        , commands    :: [Entity]
        , producers   :: [Entity]
        , newEntities :: [Entity]
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


updateProducers :: GameState -> GameState
updateProducers gs = gs { producers     = map fst updated
                        , newEntities   = concat (map snd updated)
                        } where updated = map updateProducer (producers gs)


updateCommands :: GameState -> GameState
updateCommands gs = gs { commands      = map fst updated
                       , newEntities   = concat (map snd updated)
                       } where updated = map updateCommand (commands gs)


finishBuilding :: Entity -> GameState -> GameState
finishBuilding p@(Producer _ _ _) gs = gs { producers = p : producers gs }
finishBuilding e gs 
        | name e == SupplyDepot      = gs { gsSupplyMax = (gsSupplyMax gs) + scvProvidedSupply }
        | name e == CommandCenter    = gs { producers = e : producers gs } 
        | otherwise                  = gs



incrementTime :: GameState -> GameState
incrementTime gs = updateGameState (gs { gsTime = gsTime gs + 1 })


mineralsMined :: GameState -> Double
mineralsMined gs = foldr ((+) . mined) 0 (commands gs) 
                   where mined = mineralsMined' . numMiners


collectMining :: GameState -> GameState
collectMining gs = gs { gsMins = gsMins gs + mineralsMined gs }


updateGameState :: GameState -> GameState
updateGameState = collectMining . updateProducers . updateCommands


