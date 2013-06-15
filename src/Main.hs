module Main where
import Entities                       
import GameState     
import CommonInterface      
import CommandBuilding
import System.Environment  (getArgs)

interactWith :: (String -> String) -> FilePath -> FilePath -> IO ()
interactWith function inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile (function input)

main :: IO ()
main = mainWith fixLines
  where mainWith function = do
          args <- getArgs
          case args of
            [input,output] -> interactWith function input output
            _ -> putStrLn "error: exactly two arguments needed"


splitLines :: String -> [String]
splitLines [] = []
splitLines cs =
    let (pre, suf) = break isLineTerminator cs
    in  pre : case suf of
                ('\r':'\n':rest) -> splitLines rest
                ('\r':rest)      -> splitLines rest
                ('\n':rest)      -> splitLines rest
                _                -> []


isLineTerminator :: Char -> Bool
isLineTerminator c = c == '\r' || c == '\n'

fixLines :: String -> String
fixLines input = unlines (splitLines input)


data GameEvent = GameEvent { evName      :: EName
                           , evMins      :: Double
                           , evGas       :: Double
                           , evSupply    :: Int
                           , evSupplyMax :: Int
                           , evTime      :: Int
                           } deriving Show

-- Don't want to keep track of game entities
gameEvent :: EName -> GameState -> GameEvent
gameEvent n gs = GameEvent n (gsMins gs) (gsGas gs) (gsSupply gs) (gsSupplyMax gs) (gsTime gs)

nilEvent :: GameEvent
nilEvent = GameEvent Nil 0 0 0 0 0

nilEvent' :: GameState -> GameEvent
nilEvent' gs = GameEvent Nil (gsMins gs) (gsGas gs) (gsSupply gs) (gsSupplyMax gs) (gsTime gs)


notNil :: GameEvent -> Bool
notNil (GameEvent Nil _ _ _ _ _) = False
notNil _                         = True

removeNilEvents :: [GameEvent] -> [GameEvent]
removeNilEvents ges = filter notNil ges


terranStart :: GameState
terranStart = GameState 50 0 6 11 0 [create CommandCenter'] [] []

myList = [Scv, Scv, Scv, Scv]

runGame = removeNilEvents (gameLoop myList terranStart 100)
runGame2 =  (gameLoop myList terranStart 100)

try1 = tryBuild Scv


gameLoop :: [EName] -> GameState -> Int -> [GameEvent]
gameLoop _                  gs attemptsLeft
        | gsTime gs    == 600 = []
        | attemptsLeft == 0   = []
gameLoop list@(n:buildList) gs i
        | success             = gameEvent n gs2 : gameLoop buildList gs2 100
        | otherwise           = nilEvent' gs2 : gameLoop (list) (incrementTime gs2) (i - 1)
        where r       = tryBuild n gs
              gs2     = snd r
              success = fst r
gameLoop _            _  _    = []


tryBuild :: EName -> GameState -> (Bool, GameState)
tryBuild n gs = tryBuild' (create n) gs 0


tryBuild' :: Entity -> GameState -> Int -> (Bool, GameState)
tryBuild' _ gs 100                  = (False, gs)
tryBuild' e gs _ |  canAfford  e gs
                 && hasBuilder e gs = (True, (payFor e . startBuilding e) gs)
tryBuild' e gs attempts             = tryBuild' e gs (attempts + 1)


