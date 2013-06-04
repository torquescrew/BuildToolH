
module GameState where
--import CommonInterface
import Entities

data GameState = GameState
        { gsMins      :: Double
        , gsGas       :: Double
        , gsSupply    :: Int
        , gsSupplyMax :: Int
        , gsTime      :: Int
        , gsEntities  :: [Entity]
        } deriving (Show, Eq)

supplyFree :: GameState -> Int
supplyFree gs = gsSupplyMax gs - gsSupply gs

entities :: GameState -> [Entity]
entities = gsEntities

-- Return new GameState where the given entity is being built.
startBuilding :: Entity -> GameState -> GameState
startBuilding e gs = gs { gsEntities = startBuilding' e (entities gs) }


-- Return new entity list where the given entity is being built.
startBuilding' :: Entity -> [Entity] -> [Entity]
startBuilding' _ []                               = []
startBuilding' e (p:ae) | name p `elem` builtBy e = build p (name e) (buildTime e) : ae
                        | otherwise               = p : startBuilding' e ae