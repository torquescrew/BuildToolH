module Main where
import Entities                       
import GameState     
import CommonInterface      
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


data GameEvent = GameEvent { evName  :: EName
                           , evState :: GameState
                           } deriving Show

-- Don't want to keep track of game entities
gameEvent :: EName -> GameState -> GameEvent
gameEvent n gs = GameEvent n gs { commands = [], producers = [] }


terranStart :: GameState
terranStart = GameState 50 0 6 11 0 [create CommandCenter'] [] []



gameLoop :: [EName] -> GameState -> [GameEvent] -> [GameEvent]
gameLoop (n:buildList) gameState events
                | success   = gameLoop buildList newState ((gameEvent n newState) : events)
                | otherwise = 
                where r        = tryBuild n gameState
                      newState = snd r
                      success  = fst r


tryBuild :: EName -> GameState -> (Bool, GameState)
tryBuild n gs = tryBuild' (create n) gs 0


tryBuild' :: Entity -> GameState -> Int -> (Bool, GameState)
tryBuild' _ gs 100                  = (False, gs)
tryBuild' e gs _ |  canAfford  e gs
                 && hasBuilder e gs = (True, (payFor e . startBuilding e) gs)
tryBuild' e gs attempts             = tryBuild' e gs (attempts + 1)


