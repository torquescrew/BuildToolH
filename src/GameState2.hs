{-# LANGUAGE TemplateHaskell #-}

module GameState2 where
import Control.Lens

data GameState = GameState
        { _mins :: Double
        
        } deriving Show


makeClassy ''GameState