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
-- -- R
-- -- RRRRYRR
-- -- RYRYRYR
-- -- RYRYRYR
-- -- RYRYRYR
-- 

readGame :: String -> Game
readGame = undefined

showGame :: Game -> String
showGame (board, color) =
    let
        colNumbers = " 1 2 3 4 5 6 7"
        separator = "-----------------"
        boardText = printBoard board
        turn = "Current turn: " ++ [showColor color]
    in
        unlines [colNumbers, separator, boardText, separator, turn]