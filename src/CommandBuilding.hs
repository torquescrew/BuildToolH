module CommandBuilding where
import Entities



updateCommand :: Entity -> (Entity, [Entity])
updateCommand c@(Command _ Nil _ _) = (c, [])
updateCommand c@(Command _ e   0 _) = (c { building = Nil }, [create e])
updateCommand c@(Command _ _   _ _) = (c { timeLeft = timeLeft c - 1 }, [])
updateCommand e                     = (e, [])

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
numMiners _                 = error "Asking for numMiners from non Command"


mineMins :: Entity -> Entity -> Entity
mineMins worker c@(Command _ _ _ _) = c { workers = worker : workers c }
mineMins _      _                   = error "Failed to add worker to Command"


minMiners :: [Entity] -> Int
minMiners workers' = minimum (map numMiners workers')

sendToMineMins' :: Entity -> Int -> [Entity] -> [Entity]
sendToMineMins' w n (c:cs) | numMiners c == n = mineMins w c : cs
                           | otherwise        = c : sendToMineMins' w n cs
sendToMineMins' _ _ _                         = []


--collectWorkers :: Entity
