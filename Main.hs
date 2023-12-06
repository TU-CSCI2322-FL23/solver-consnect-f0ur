{-# HLINT ignore "Redundant bracket" #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main where

import Data.Maybe (fromJust)
import Data.Maybe qualified
import Game
import IO
import System.Console.GetOpt
import System.Environment

data Flag = Winner | Depth String | Help | Move String | Verbose | Interactive | Battle deriving (Show, Eq)

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
      "Starts an interactive game of Connect Four.",
    Option
      ['b']
      ["battle"]
      (NoArg Battle)
      "Starts a Connect Four battle between two players (pass the keyboard)",
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
      else
        if Interactive `elem` flags
          then -- start an interactive game
            if null inputs
              then playGame (emptyBoard, Game.Red) flags
              else do
                game@(board, player) <- loadGame (head inputs)
                playGame (board, Game.Red) flags
          else if Battle `elem` flags
            then 
              if null inputs
                then battleGame (emptyBoard, Game.Red) flags
                else do
                  game@(board, player) <- loadGame (head inputs)
                  battleGame (board, Game.Red) flags
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

-- takes in a list of flags and a game
-- if winner flag is in flags, print the best move and exit
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

-- Starts an interactive game of Connect Four
playGame :: Game -> [Flag] -> IO ()
playGame game flags
  | Depth x <- head flags = playGameRecur game (read x)
  | otherwise = playGameRecur game 3

playGameRecur :: Game -> Int -> IO ()
playGameRecur game depth =
  -- check for win on game
  let winner = gameWin game
   in if Data.Maybe.isNothing winner
        then do
          outputBoard (fst game)
          putStrLn "Enter a column number to make a move (0-6): "
          move <- getLine
          let newGame = makeMove game (read move :: Int)
          if isValidMove game (read move :: Int)
            then case newGame of
              Just outputGame ->
                do
                  let (rating, move) = whoMightWin outputGame depth
                  let winner = gameWin outputGame
                  outputBoard (fst outputGame)
                  if (Data.Maybe.isJust winner)
                    then outputMaybeWinner winner
                  else
                    case makeMove outputGame move of
                      Just x -> playGameRecur x depth
                      Nothing -> putStrLn "Column Full"
              Nothing -> do
                putStrLn "Invalid move"
                playGameRecur game depth
            else do
              putStrLn "Invalid move"
              playGameRecur game depth
        else do
          outputBoard (fst game)
          outputMaybeWinner winner

-- Starts an interactive game of Connect Four
battleGame :: Game -> [Flag] -> IO ()
battleGame game flags
  | Depth x <- head flags = battleGameRecur game (read x)
  | otherwise = battleGameRecur game 3

battleGameRecur :: Game -> Int -> IO ()
battleGameRecur game depth =
  -- check for win on game
  let winner = gameWin game
   in if Data.Maybe.isNothing winner
        then do
          outputBoard (fst game)
          putStrLn $ "Its " ++ show (snd game) ++ "'s turn to enter a column number to make their move (0-6): "
          move <- getLine
          let newGame = makeMove game (read move :: Int)
          case newGame of
              Just outputGame -> battleGameRecur outputGame depth
              Nothing -> do
                putStrLn "Invalid move"
                battleGameRecur game depth
      else do
      outputBoard (fst game)
      outputMaybeWinner winner

outputMaybeWinner :: Maybe Winner -> IO()
outputMaybeWinner (Just (Win Game.Yellow)) = putStrLn "Yellow Wins!!!"
outputMaybeWinner (Just (Win Game.Red)) = putStrLn "Red Wins!!!"
outputMaybeWinner (Just Stalemate) = putStrLn "Stalemate...Sorry Folks"

-- Returns true if the Verbose flag is present in the list of flags
hasVerbose :: [Flag] -> Bool
hasVerbose flags = Verbose `elem` flags