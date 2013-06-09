module Producers where
import Entities

--
--updateProducers :: [Entity] -> [Entity]
--updateProducers (p:producers) = updateProducer p ++ updateProducers

updateProducer :: Entity -> [Entity]
updateProducer (Producer stats e remaining) 
                              | remaining == 0 = [Producer stats Nil 0, create e]
                              | otherwise      = [Producer stats e (remaining - 1)]
updateProducer p                               = [ p ]                  


updateProducer' :: Entity -> [Entity]
updateProducer' (Producer stats e remaining) 
                              | remaining == 0 = [Producer stats Nil 0, create e]
                              | otherwise      = [Producer stats e (remaining - 1)]
updateProducer' p                              = [ p ]  