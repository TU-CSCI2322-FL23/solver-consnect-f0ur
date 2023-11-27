module Main where
import Game
import IO


import GHC.IO

main :: IO ()
main = do
    putStrLn "Welcome to Connect Four!"
    putStrLn "Enter the path to a game file to load it."
    putStr "File to load --> "
    filePath <- getLine
    game <- loadGame filePath
    putBestMove game

