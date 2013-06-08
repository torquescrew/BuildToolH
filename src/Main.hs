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



terranStart :: GameState
terranStart = GameState 50 0 6 11 0 [create CommandCenter'] []



----gameLoop :: Entity -> GameState -> GameState
----gameLoop e gameState = event

tryBuild :: Entity -> GameState -> (Bool, GameState)
tryBuild e gs = tryBuild' e gs 0


tryBuild' :: Entity -> GameState -> Int -> (Bool, GameState)
tryBuild' _ gs 100                  = (False, gs)
tryBuild' e gs _ |  canAfford  e gs
                 && hasBuilder e gs = (True, (payFor e . startBuilding e) gs)
tryBuild' e gs attempts             = tryBuild' e gs (attempts + 1)


