module Main where

import Data.Maybe (fromJust)
import Data.Maybe qualified
import Game
import IO
import System.Console.ANSI
import System.Console.GetOpt
import System.Environment

data Flag = Winner | Depth String | Help | Move String | Verbose | Interactive deriving (Show, Eq)

options :: [OptDescr Flag]
options =
  [ Option
      ['w']
      ["winner"]
      (NoArg Winner)
      "Finds the best move (with no cutoff), prints it, and exits. Overrides -d.",
    Option
      ['d']
      ["depth"]
      (ReqArg Depth "<num>")
      "Modifies the algoritm's cutoff depth (default: 3). Will do nothing if -w is specified.",
    Option
      ['h']
      ["help"]
      (NoArg Help)
      "Print helpful usage information and exit.",
    Option
      ['m']
      ["move"]
      (ReqArg Move "<num>")
      "Makes the specified move (0-6) and prints the resulting board to stdout",
    Option
      ['i']
      ["interactive"]
      (NoArg Interactive)
      "MAKE THE GAME INTERACTIVE!!! YAY!",
    Option
      ['v']
      ["verbose"]
      (NoArg Verbose)
      "Prints extra information about the game state to stdout"
  ]

main :: IO ()
main =
  do
    args <- getArgs
    let (flags, inputs, errors) = getOpt Permute options args

    if Help `elem` flags || null args
      then putStrLn $ usageInfo "Usage: connect-four [OPTION...] [FILE]" options
      else do
        -- should maybe be moved into if null flags condition
        game@(board, player) <- loadGame (head inputs)
        outputBoard board

        if null flags
          then
            let (rating, move) = whoMightWin game 3
             in returnBestMove (rating, move) game (hasVerbose flags)
          else handleFlags flags game

        return ()

-- takes in rating and move along with a bool for if verbose flag is active (true if verbose is in flags / false if it isn't in flags) *use hasverbose
-- should return the best move, and if verbose is active, print the best move and the rating plus whether it's a win or loss or tie or nothing.
returnBestMove :: (Rating, Move) -> Game -> Bool -> IO ()
returnBestMove (rating, move) game isVerbose =
  let winner = gameWin game
   in if isVerbose
        then case winner of
          Just (Win Game.Yellow) -> putStrLn $ "The best move is column #" ++ show move ++ " with a rating of " ++ show rating ++ ". This is a win!"
          Just (Win Game.Red) -> putStrLn $ "The best move is column #" ++ show move ++ " with a rating of " ++ show rating ++ ". This is a loss!"
          Just Stalemate -> putStrLn $ "The best move is column #" ++ show move ++ " with a rating of " ++ show rating ++ ". This is a tie!"
          Nothing -> putStrLn $ "The best move is column #" ++ show move ++ " with a rating of " ++ show rating
        else putStrLn $ "The best move is column #" ++ show move ++ "."

handleFlags :: [Flag] -> Game -> IO ()
handleFlags flags game@(board, player)
  | Winner `elem` flags = putStrLn ("The best move is " ++ show (bestMove game))
  | Move move <- head flags =
      let hasVerbose = Verbose `elem` flags
       in if hasVerbose
            then case makeMove game (read move :: Int) of
              Just outputGame -> putStrLn $ showGame outputGame
              Nothing -> putStrLn "Invalid move"
            else case makeMove game (read move :: Int) of
              Just outputGame -> outputBoard (fst outputGame)
              Nothing -> putStrLn "Invalid move"
  | Depth x <- head flags =
      let (rating, move) = whoMightWin game (read x)
       in returnBestMove (rating, move) game (hasVerbose flags)
  | Verbose `elem` flags =
      let (rating, move) = whoMightWin game 3
       in returnBestMove (rating, move) game True
  | otherwise = return ()

hasVerbose :: [Flag] -> Bool
hasVerbose flags = Verbose `elem` flags

-- case flag of
-- Winner -> putStrLn ("The best move is " ++ show (bestMove game))
-- Help -> putStrLn $ usageInfo "Usage: connect-four [OPTION...] [FILE]" options
-- Depth depth -> let (rating, move) = whoMightWin game (read depth :: Int)
--         in putStrLn $ "The best move is " ++ show move ++ " with a rating of " ++ show rating
-- _ -> putStrLn "AHHHHHH!!!! ERRROR!!!! AAHAHHAA"

-- if Help `elem` flags
-- then
--     if not $ null inputs
--     then putStrLn $ usageInfo "Usage: connect-four [OPTION...] [FILE]" options
--     else putStrLn $ "Please input a file.\n" ++ usageInfo "Usage: connect-four [OPTION...] [FILE]" options
-- else if not $ null inputs
-- then
--     --let (rating, move) = whoWillWin (loadGame (head inputs)) 3
--         --in putStrLn $ "The best move is " ++ show move ++ " with a rating of " ++ show rating
--     let moves = game
--         in putStrLn $ "The best move is " ++ showGame game ++ " with a rating of " ++ "NA"
-- else
--     putStrLn "AHHHHHH!!!! ERRROR!!!! AAHAHHAA"

-- else if Winner `elem` flags
-- then
--     putStrLn "winner winner chicken dinner!"
-- else if Interactive `elem` flags
-- then
--     putStrLn "interactive interactive chicken interactive!"
-- else if Verbose `elem` flags
-- then
--     putStrLn "verbose verbose chicken verbose!"
-- else if Move "STRING!" `elem` flags
-- then
--     putStrLn "move move chicken move!"
-- else if Depth "STRING!" `elem` flags
-- then
--     putStrLn "depth depth chicken depth!"
-- else
--     putStrLn "no flags chicken no flags!"

{-

setSGR [SetColor Foreground Vivid Green]
putStrLn "    Welcome to Connect Four!"
putStrLn "--------------------------------"
setSGR [Reset]
putStrLn "Enter the path to a game file to load it."
putStr "File to load --> "
filePath <- getLine
game <- loadGame filePath
putBestMove game

-}