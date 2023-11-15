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

testGame :: Game
testGame = (testBoard, Red)

readGame :: String -> Game
readGame = undefined

showGame :: Game -> String
showGame game =
    let
        board = fst game
        player = snd game
        showRow row = [showColor color | color <- row]
    in
        -- append the player, then each row of the board
        showColor player : concatMap showRow board