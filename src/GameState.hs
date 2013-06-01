
module GameState where
--import CommonInterface
--import Entities

data GameStats = GStats
        { mins'      :: Double
        , gas'       :: Double
        , supply'    :: Int
        , supplyMax  :: Int
        , time       :: Int
        } deriving (Show, Eq)

supplyFree :: GameStats -> Int
supplyFree gs = supplyMax gs - supply' gs

