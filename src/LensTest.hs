{-# LANGUAGE TemplateHaskell, RankNTypes, NoMonomorphismRestriction, KindSignatures #-}

module LensTest where
import Control.Lens


data Inner = Inner
        { _mins :: Double } deriving Show

data Outer = Outer
        { _inner :: Inner } deriving Show


--getLatitude :: Location -> Arc
--getLatitude = view latitude 

makeLenses '' Inner
makeLenses '' Outer

--getInner :: Outer -> Inner
--getInner = view inner

--myMins :: Outer -> Double
--myMins a = _mins (_inner a)

--myMins :: Inner -> Double
--myMins  = _mins
--
--mmm :: Integer
--mmm = 5

--_mins2 :: Inner -> Double

--colorCalc :: Int -> (CustomColor, Int)

-- makeLensesFor [("_foo", "fooLens"), ("baz", "lbaz")] ''Foo
-- makeClassyFor "HasFoo" "foo" [("_foo", "fooLens"), ("bar", "lbar")] ''Foo


--setMins :: Outer -> Double -> Outer 
--setMins o m = o & inner.mins .~ m

--makeLensesFor [("myMins", "mins")] ''Inner

--myMins :: Outer -> Double
--myMins a = a^.outer.mins


--d = Inner 4.0

a :: Outer
a = Outer (Inner 5.0)

getMins :: forall (p :: * -> * -> *) (f :: * -> *).
                      (Functor f, Profunctor p) =>
                      p Double (f Double) -> p Outer (f Outer)
getMins = inner . mins


--setMins = inner & mins

something :: Double
something = a^.getMins

--c = view inner

--b = a ^. inner . mins


data EName = Scv
           | SupplyDepot
           | CommandCenter
           | Barracks
           | Marine
           | Nil
             deriving (Show, Eq, Enum, Read)

data BaseStats = Stats
               { _name'      :: EName
               , _mins'      :: Double
               , _gas'       :: Double
               , _buildTime' :: Int
               , _supply'    :: Int
               , _builtBy'   :: [EName]
               } deriving (Show, Eq)


data PAttr = PAttr
             { building' :: EName
             , timeLeft' :: Int
             } deriving (Show, Eq)
             
makeLenses ''BaseStats



data Entity stats pAttr = Ent stats | Pro stats pAttr deriving Show

e :: forall pAttr. Entity BaseStats pAttr
e = Ent (Stats SupplyDepot   100 0  30 0 [Scv])


getName :: forall t. Entity BaseStats t -> EName
getName (Ent (Stats n _ _ _ _ _ ) ) = n
getName _ = Nil


--get :: Maybe Entity -> Entity
--get (




