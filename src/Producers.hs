module Producers where
import Entities
--import GameState


updateProducer :: Entity -> (Entity, [Entity])
updateProducer p@(Producer _ Nil _) = (p, [])
updateProducer p@(Producer _ e   0) = (p { building = Nil }, [create e])
updateProducer p@(Producer _ _   _) = (p { timeLeft = timeLeft p - 1 }, [])
updateProducer e                    = (e, [])
--updateProducer (Producer stats e remaining) = (Producer stats e (remaining - 1), [])
--updateProducer p                            = (p, [])



--updateProducers :: GameState -> GameState
--updateProducers gs = gs { producers = map fst updated, newEntities = concat (map snd updated) }
--                        where updated = map updateProducer (producers gs)
--
--
--finishBuilding :: Entity -> GameState -> GameState
--finishBuilding p@(Producer _ _ _) gs = gs { producers = p : producers gs }
--finishBuilding e gs 
--        | name e == SupplyDepot      = gs { gsSupplyMax = (gsSupplyMax gs) + scvProvidedSupply }
--        | name e == CommandCenter    = gs { producers = e : producers gs } 
--        | otherwise                  = gs







                        
                        