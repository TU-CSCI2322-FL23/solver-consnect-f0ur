module IO where

import Game
import GHC.IO
import Game (printBoard)

main :: IO ()
main = do
    putStrLn "Hello, what is your name?"
    name <- getLine
    putStrLn ("Hello " ++ name ++ "!")


outputBoard :: Board -> IO ()
outputBoard board = putStrLn $ printBoard board

-- 
-- -- Output formnat:
-- -- 
-- 

readGame :: String -> Game
readGame = undefined

showGame :: Game -> String
showGame = undefined