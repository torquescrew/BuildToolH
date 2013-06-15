module Producers where
import Entities
--import GameState


updateProducer :: Entity -> (Entity, [Entity])
updateProducer p@(Producer _ Nil _) = (p, [])
updateProducer p@(Producer _ e   0) = (p { building = Nil }, [create e])
updateProducer p@(Producer _ _   _) = (p { timeLeft = timeLeft p - 1 }, [])
updateProducer e                    = (e, [])


