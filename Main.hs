module Main where
import Game
import IO
import System.Console.GetOpt
import System.Environment

import System.Console.ANSI


import GHC.IO

data Flag = Winner | Depth String | Help | Move String | Verbose | Interactive deriving (Show, Eq)

options :: [OptDescr Flag]
options = [
    Option ['w'] ["winner"] (NoArg Winner)
        "Finds the best move (with no cutoff), prints it, and exits. Overrides -d.",
    Option ['d'] ["depth"] (ReqArg Depth "<num>")
        "Modifies the algoritm's cutoff depth (default: 3). Will do nothing if -w is specified.",
    Option ['h'] ["help"] (NoArg Help)
        "Print helpful usage information and exit.",
    Option ['m'] ["move"] (ReqArg Move "<num>")
        "Makes the specified move and prints the resulting board to stdout",
    Option ['i'] ["interactive"] (NoArg Interactive)
        "MAKE THE GAME INTERACTIVE!!! YAY!"
    ]

main :: IO ()
main =
 do args <- getArgs
    let (flags, inputs, errors) = getOpt Permute options args
    if Help `elem` flags || not (null error)
    then
        if not $ null inputs
        then putStrLn $ usageInfo "Usage: connect-four [OPTION...] [FILE]" options
        else putStrLn $ "Please input a file.\n" ++ usageInfo "Usage: connect-four [OPTION...] [FILE]" options
    else if Winner `elem` flags || not (null error)
    then
        putStrLn "winner winner chicken dinner!"
    else if Interactive `elem` flags || not (null error)
    then
        putStrLn "interactive interactive chicken interactive!"
    else if Move `elem` flags || not (null error)
    then
        putStrLn "move move chicken move!"
    else if Depth `elem` flags || not (null error)
    then
        putStrLn "depth depth chicken depth!"
    else
        putStrLn "no flags chicken no flags!"









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