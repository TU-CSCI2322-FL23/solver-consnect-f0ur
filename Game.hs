{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use <=" #-}

module Game where

import Data.List (transpose, isInfixOf)
import Data.List.Extra (intersperse)
import Data.Maybe (fromMaybe, catMaybes)


-- Types -------------------------------------------------------------------------------------------------

-- Disk = what you play with
-- Color = what gets dropped in /Player
-- tile = indiviual holes in the board
-- board 7 columns 6 rows, 42 holes in totßal

-- Column number (inputed by player when making move)
-- Only storing colors in holes; not storing empty holes
-- 7 Columns by default
-- Head of column is highest color in stack
--     - when we drop a new color, it gets appended to the front of the list
-- Don't need to index bc we can use  splitat
type Column = [Color]

-- Possibilities of states for the holes on the board
-- We don't derive Show because we have a custom one!
data Color = Yellow | Red deriving (Eq, Show)

data Winner = Win Color | Stalemate deriving (Eq, Show) -- define win

-- List of the states for the holes for the whole board
type Board = [Column]

-- Board in game and the color of the players turn
type Game = (Board, Color)

-- 0 to 6 (makes it easier to code; we can change it to 1 to 7 later)
type Move = Int



-- Make the board -------------------------------------------------------------------------------------------------

emptyColumn :: Column
emptyColumn = []

emptyBoard :: Board
emptyBoard = replicate 7 emptyColumn

-- Making the game -----------------------------------------------------------------------------------------------

--checks if the column is full
columnFull :: Column -> Bool
columnFull givenColumn = length givenColumn == 6

isMoveOutofBounds :: Move -> Bool
isMoveOutofBounds x = x < 0 || x > 6

-- may need to add check for valid move
makeMove :: Game -> Move -> Maybe Game
makeMove (currentBoard, moveColor) x =
    if isMoveOutofBounds x
        then Nothing
    else
        let
            (before, inclusiveAfter) = splitAt x currentBoard
        in
            if columnFull (head inclusiveAfter)
                then Nothing
            else
                Just (before ++ [moveColor : head inclusiveAfter] ++ tail inclusiveAfter, swapColor moveColor)

-- takes in color and returns other color
swapColor :: Color -> Color
swapColor color = if color == Red then Yellow else Red


--checks if the entire board is full, indicating a draw
boardFull :: Board -> Bool
boardFull board = all columnFull board

--iterates over all the columns and filters out all the valid moves  
validMoves :: Game -> [Move]
validMoves (board, turn) = --filter (isValidMove (board, turn)) [0..length board - 1]
    [column | (column, _) <- zip [0..] board, isValidMove (board, turn) column]

--helper function for validmoves
isValidMove :: Game -> Move -> Bool
isValidMove (board, turn) column
    | column < 0 || column >= length board = False --out of bounds column index 
    | length (board !! column) >= 6 = False --checks if the column is full 
    | otherwise = True


-- Win Methods CAG  --------------------------------------------------------------------------------------------

-- Determines the winner of the game based on the current game state.
gameWin :: Game -> Maybe Winner
gameWin (board, color)
  | horizontalWin board color || verticalWin board color || diagonalWin board color = Just $ Win color
  | boardFull board = Just Stalemate
  | otherwise = Nothing

--checks both colors for testing purposes
unsafeGameWin :: Game -> Winner
unsafeGameWin (board, color) =
    let
        otherCol = swapColor color
    in
        if horizontalWin board color || verticalWin board color || diagonalWin board color
            then Win color
        else if horizontalWin board otherCol || verticalWin board otherCol || diagonalWin board otherCol
            then Win otherCol
        else Stalemate

-- Gets list of colors in all rows of board and checks if any of them has four of given color in a row
horizontalWin :: Board -> Color -> Bool
horizontalWin board Red = or [ [Just Red, Just Red, Just Red, Just Red] `isInfixOf` x | x <-horizontalWinHelper board]
horizontalWin board Yellow = or [ [Just Yellow, Just Yellow, Just Yellow, Just Yellow] `isInfixOf` x | x <- horizontalWinHelper board]

-- Gets list of colors in each row for full board
horizontalWinHelper :: Board -> [[Maybe Color]]
horizontalWinHelper board = [getRowFromBoard board x | x <- [0..5]]

-- Gets list of colors in given row
getRowFromBoard :: Board -> Int -> [Maybe Color]
getRowFromBoard board n = [ getRowFromColumn (reverse x) n | x <- board]

-- Returns Color at the index of given column (returns nothing if it isn't present)
getRowFromColumn :: Column -> Int -> Maybe Color
getRowFromColumn col n =
    case drop n col of
        [] -> Nothing
        (x:xs) -> Just x

-- Checks if there are four of a row in each column 
verticalWin :: Board -> Color -> Bool
verticalWin board Red = or [ [Red, Red, Red, Red] `isInfixOf` x | x <- board]
verticalWin board Yellow = or [ [Yellow, Yellow, Yellow, Yellow] `isInfixOf` x | x <- board]

-- Gets list of all diagonals in board and checks if any of them contain four of given color
diagonalWin :: Board -> Color -> Bool
diagonalWin board Red = or [ [Just Red, Just Red, Just Red, Just Red] `isInfixOf` x | x <-getDiagonalsFromBoard board]
diagonalWin board Yellow = or [ [Just Yellow, Just Yellow, Just Yellow, Just Yellow] `isInfixOf` x | x <- getDiagonalsFromBoard board]

-- Returns list of all diagonals (of length 4) in board
getDiagonalsFromBoard :: Board -> [[Maybe Color]]
getDiagonalsFromBoard board =
    let
        [one,two,three,four] = groupsOfFourColumns board
    in
        groupsOfFourColumnsDiagonals one ++ groupsOfFourColumnsDiagonals two ++ groupsOfFourColumnsDiagonals three ++ groupsOfFourColumnsDiagonals four

--Returns all groups of four columns (with columns in reverse) (assumes 7 columns)
groupsOfFourColumns :: Board -> [[Column]]
groupsOfFourColumns [first,second,third,fourth,fifth,sixth,seventh] =
    let
        one   = [reverse first,reverse second,reverse third,reverse fourth]
        two   = [reverse second,reverse third,reverse fourth,reverse fifth]
        three = [reverse third,reverse fourth,reverse fifth,reverse sixth]
        four  = [reverse fourth,reverse fifth,reverse sixth,reverse seventh]
    in
        [one,two,three,four]

-- Returns all diagonals in group of four columns
groupsOfFourColumnsDiagonals :: [Column] -> [[Maybe Color]]
groupsOfFourColumnsDiagonals columns = leftDownDiagonals columns ++ rightDownDiagonals columns

-- Takes list of four columns and finds all the diagonals that start with the left side down (max of 3 output)
leftDownDiagonals :: [Column] -> [[Maybe Color]]
leftDownDiagonals [first, second, third, fourth] =
    let
        bottom = [getRowFromColumn first 0, getRowFromColumn second 1, getRowFromColumn third 2, getRowFromColumn fourth 3]
        middle = [getRowFromColumn first 1, getRowFromColumn second 2, getRowFromColumn third 3, getRowFromColumn fourth 4]
        top    = [getRowFromColumn first 2, getRowFromColumn second 3, getRowFromColumn third 4, getRowFromColumn fourth 5]
    in
        [bottom, middle, top]

-- Takes list of four columns and finds all the diagonals that start with the right side down (max of 3 output)
rightDownDiagonals :: [Column] -> [[Maybe Color]]
rightDownDiagonals [first, second, third, fourth] =
    let
        bottom = [getRowFromColumn first 3, getRowFromColumn second 2, getRowFromColumn third 1, getRowFromColumn fourth 0]
        middle = [getRowFromColumn first 4, getRowFromColumn second 3, getRowFromColumn third 2, getRowFromColumn fourth 1]
        top    = [getRowFromColumn first 5, getRowFromColumn second 4, getRowFromColumn third 3, getRowFromColumn fourth 2]
    in
        [bottom, middle, top]

-- Reverse the columns in a board.
reverseColumns :: Board -> Board
reverseColumns = map reverse

-- Determining optimal move ------------------------------------------------------------------------------------------------------------------------

-- Takes a game (close to being over) and returns if the current color can force a win or a stalemate
whoWillWin :: Game -> Winner
whoWillWin game@(board, color) = 
    case gameWin game of 
        Just outcome -> outcome
        Nothing -> 
            let moves = validMoves game
                childGames = catMaybes [ makeMove game x | x <- moves]
                winners = map whoWillWin childGames
            in if Win color `elem` winners 
               then Win color
               else if Stalemate `elem` winners
                then Stalemate
                else Win $ swapColor color

bestMove :: Game -> Move
bestMove game@(board, color) = 
    let moves = validMoves game
        childGames = catMaybes [ makeMove game x | x <- moves]
        winners = map bestMove childGames
    in if Win color `elem` winners 
       then Win color
       else if Stalemate `elem` winners
        then Stalemate
        else Win $ swapColor color

-- test board for debugging
testBoard :: Board
testBoard = [
    [Yellow, Red, Yellow, Red, Yellow, Red],
    [Red, Yellow, Red, Yellow, Red, Yellow],
    [Yellow, Red, Yellow, Red, Yellow, Red],
    [Red, Yellow, Red, Yellow, Red, Yellow],
    [Yellow, Red, Yellow, Red, Yellow, Red],
    [Red, Yellow, Red, Yellow, Red, Yellow],
    [Yellow, Red, Yellow, Red, Yellow, Red]
    ]

winningBoard :: Board
winningBoard = [
    [Yellow, Yellow, Yellow, Yellow, Red, Red, Red],
    [Red, Red, Red, Yellow, Red, Yellow, Yellow],
    [Yellow, Red, Yellow, Red, Yellow, Red, Red],
    [Red, Yellow, Red, Yellow, Red, Yellow, Yellow],
    [Yellow, Yellow, Yellow, Yellow, Red, Red, Red],
    [Red, Red, Red, Red, Yellow, Yellow, Yellow],
    [Yellow, Red, Yellow, Red, Yellow, Red, Yellow]
    ]

--test board for validMoves
validMovesBoard :: Board
validMovesBoard = [
                 [Yellow, Red, Yellow, Red],
            [Red, Yellow, Red, Red, Yellow],
            [Red, Yellow, Red, Yellow, Red],
            [Red, Yellow, Red, Red, Yellow],
         [Yellow, Yellow, Red, Yellow, Red],
                         [Red, Red, Yellow],
    [Yellow, Red, Yellow, Red, Yellow, Red]]

-- Prints the board in a human-readable format.
-- This is the function we should call elsewhere. (fyi)
printBoard :: Board -> String
printBoard board = unlines $ map (intersperse '|') $ reverse $ transpose $ padBoard board
    where
        padBoard board = map padCol board
        padCol col = colToString col ++ replicate (6 - length col) ' '


-- Prints a given column of colors in reverse. This is just so that we can rotate the board 90 degrees and everything still lines up.
colToString :: [Color] -> [Char]
colToString [] = ""
colToString lst = foldr (\x y -> showColor x : y) "" $ reverse lst

-- Returns Y for Yellow and R for Red.
showColor :: Color -> Char
showColor Yellow = 'Y'
showColor Red = 'R'

showWinner :: Winner -> String
showWinner (Win Yellow) = "Yellow"
showWinner (Win Red) = "Red"
showWinner Stalemate = "Stalemate"