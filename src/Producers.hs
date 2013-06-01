module Producers where
import Entities


updateProducer :: Entity -> [Entity]
--updateProducer (Producer stats Nil _)          = [Producer stats Nil 0 ]
updateProducer (Producer stats e remaining) 
                              | remaining == 0 = [Producer stats Nil 0, create e]
                              | otherwise      = [Producer stats e (remaining - 1)]
updateProducer p                               = [ p ]                  