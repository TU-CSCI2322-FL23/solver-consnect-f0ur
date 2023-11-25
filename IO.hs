module IO where

-- Not sure why, but the bestMove function couldn't be used without importing
-- all of these manually. If we can figure out how to import everything that might
-- be a little nicer. -SV
import Game
    ( printBoard,
      showColor,
      testBoard,
      Board,
      Color(Yellow, Red),
      Game,
      bestMove,
      printBoard, validMovesBoard )

import GHC.IO

main :: IO ()
main = do
    putStrLn "Welcome to Connect Four!"
    putStrLn "Enter the path to a game file to load it."
    putStr "File to load --> "
    filePath <- getLine
    game <- loadGame filePath
    putBestMove game

-- IO Functions (Section 2) -----------------

outputBoard :: Board -> IO ()
outputBoard board = putStrLn $ printBoard board

writeGame :: Game -> FilePath -> IO ()
writeGame game path = writeFile path (showGame game)

loadGame :: FilePath -> IO Game
loadGame path = do
    contents <- readFile path
    return (readGame contents)

-- TODO: For full credit, also print the outcome that moves forces.
putBestMove :: Game -> IO ()
putBestMove game = do
    let move = bestMove game
    putStrLn ("Best move: column #" ++ show move)

-- 
-- -- Output formnat:
-- -- R
-- -- RRRRYRR
-- -- RYRYRYR
-- -- RYRYRYR
-- -- RYRYRYR
-- 

testGame :: Game
testGame = (validMovesBoard, Red)

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