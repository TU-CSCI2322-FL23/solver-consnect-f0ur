module IO where

import Game
import GHC.IO
import Game (printBoard)

main :: IO ()
main = do
    putStrLn "Hello, what is your name?"
    name <- getLine
    putStrLn ("Hello " ++ name ++ "!")

-- IO Functions (Section 2) -----------------

outputBoard :: Board -> IO ()
outputBoard board = putStrLn $ printBoard board

writeGame :: Game -> FilePath -> IO ()
writeGame game path = writeFile path (showGame game)

loadGame :: FilePath -> IO Game
loadGame path = do
    contents <- readFile path
    return (readGame contents)

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
readGame (player:board) =
    let
        readColor 'R' = Red
        readColor 'Y' = Yellow
        readColor _ = error "Invalid color"

    in
        (map (map readColor) (lines board), readColor player)


showGame :: Game -> String
showGame game =
    let
        board = fst game
        player = snd game
        showRow row = [showColor color | color <- row]
    in
        -- append the player, then each row of the board
        showColor player : unlines (map showRow board)