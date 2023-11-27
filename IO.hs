module IO where

-- Not sure why, but the bestMove function couldn't be used without importing
-- all of these manually. If we can figure out how to import everything that might
-- be a little nicer. -SV
import Game
    ( showColor,
      testBoard,
      Board,
      Color(Yellow, Red),
      Game,
      bestMove,
      validMovesBoard )

import GHC.IO
import Data.List (intersperse, transpose)

main :: IO ()
main = do
    putStrLn "Welcome to Connect Four!"
    putStrLn "Enter the path to a game file to load it."
    putStr "File to load --> "
    filePath <- getLine
    game <- loadGame filePath
    putBestMove game

-- IO Functions (Section 2) -----------------

--
-- -- Output formnat: (First color is the player whose turn it is, then it's just the board with \n to separate rows)
-- -- RRRRRYRR
-- -- RYRYRYR
-- -- RYRYRYR
-- -- RYRYRYR
--

outputBoard :: Board -> IO ()
outputBoard board = putStrLn $ printBoard board

writeGame :: Game -> FilePath -> IO ()
writeGame game path = writeFile path (showGame game)

loadGame :: FilePath -> IO Game
loadGame path = do
    contents <- readFile path
    return (readGame contents)

-- IO action to output a best move.
putBestMove :: Game -> IO ()
putBestMove game = do
    let move = bestMove game
    putStrLn ("Best move: column #" ++ show move)

-- Reads a game from a string using the output format we defined on line 30.
readGame :: String -> Game
readGame (player:board) =
    let
        readColor 'R' = Red
        readColor 'Y' = Yellow
        readColor _ = error "Invalid color"

    in
        (map (map readColor) (lines board), readColor player)

-- Outputs a game using the output format from line 30.
showGame :: Game -> String
showGame game =
    let
        board = fst game
        player = snd game
        showRow row = [showColor color | color <- row]
    in
        -- append the player, then each row of the board
        showColor player : unlines (map showRow board)

-- Converts the board to a nice string.
-- If you want to print a pretty version to IO, use the IO wrapper instead.
printGame :: Game -> String
printGame (board, color) = printBoard board

-- Converts a board to a nice string.
printBoard :: Board -> String
printBoard board = unlines $ map (intersperse '|') $ reverse $ transpose $ padBoard board
  where
    padCol col = colToString col ++ replicate (6 - length col) ' '
    padBoard = map padCol

-- Prints a given column of colors in reverse. This is just so that we can rotate the board 90 degrees and everything still lines up.
colToString :: [Color] -> [Char]
colToString [] = ""
colToString lst = foldr (\x y -> showColor x : y) "" $ reverse lst