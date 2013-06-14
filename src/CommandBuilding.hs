module CommandBuilding where
--import GameState
import Entities


--updateCommand :: Entity -> [Entity]
--updateCommand (Command stats e remaining w) 
--              | remaining == 0 = [Command stats Nil 0 w, create e]
--              | otherwise      = [Command stats e (remaining - 1) w]
--updateCommand c                = [ c ] 


updateCommand :: Entity -> (Entity, [Entity])
updateCommand c@(Command _ Nil _ _) = (c, [])
updateCommand c@(Command _ e   0 _) = (c, [create e])
updateCommand c                     = (c { timeLeft = timeLeft c - 1 }, [])

-- TODO: improve this
mineralsMined'' :: Double -> Double
mineralsMined'' wrkrs 
        | wrkrs >= 24 = 816.0 / 60.0
        | wrkrs <= 16 = (40.0 * wrkrs) / 60.0
        | otherwise   = (672.0 / 60.0) + ((wrkrs-16) * 18.0) / 60.0

mineralsMined' :: Int -> Double
mineralsMined' w = mineralsMined'' (fromIntegral w :: Double)


numMiners :: Entity -> Int
numMiners (Command _ _ _ w) = length w
numMiners  _                = 0


--mineralsMined :: GameState -> Double
--mineralsMined gs = foldr ((+) . mined) 0 (commands gs) 
--                   where mined = mineralsMined' . numMiners
--
--
--collectMining :: GameState -> GameState
--collectMining gs = gs { gsMins = gsMins gs + mineralsMined gs }

