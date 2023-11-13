{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Use <=" #-}

module Game where

import Data.List (transpose, isInfixOf)
import Data.List.Extra (intersperse)
import Data.Maybe ()

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
gameWin :: Game -> Winner
gameWin (board, color) =
    if horizontalWin board color || verticalWin board color || diagonalWin board color
        then Win color
        else Stalemate

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

-- Group the colors in a column into groups of 4 to check for consecutive colors.
group4 :: Column -> [Column]
group4 [] = []
group4 column
    | length column < 4 = []  -- If there are less than 4 colors, there can't be a group of 4.
    | otherwise = take 4 column : group4 (tail column)


-- Gets list of colors in all rows of board and seeing if any of them has four of given color in a row
horizontalWin :: Board -> Color -> Bool
horizontalWin board Red = or [ [Just Red, Just Red, Just Red, Just Red] `isInfixOf` x | x <-horizontalWinHelper board]
horizontalWin board Yellow = or [ [Just Yellow, Just Yellow, Just Yellow, Just Yellow] `isInfixOf` x | x <- horizontalWinHelper board]

-- Gets list of colors in given row for full board
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

-- Check for diagonal wins on the game board.
diagonalWin :: Board -> Color -> Bool
diagonalWin board color =
    any (\row -> checkConsecutiveDiagonals color row || checkConsecutiveDiagonals color (reverse row)) board

-- Check for consecutive diagonals of the same color.
checkConsecutiveDiagonals :: Color -> Column -> Bool
checkConsecutiveDiagonals _ [] = False
checkConsecutiveDiagonals color row
    | length row < 4 = False
    | consecutive color (take 4 row) = True
    | otherwise = checkConsecutiveDiagonals color (tail row)

-- Check if a list has consecutive pieces of the same color.
consecutive :: Color -> [Color] -> Bool
consecutive _ [] = True
consecutive _ [_] = True
consecutive c (x:y:ys)
    | x == c && y == c = consecutive c (y:ys)
    | otherwise = False

-- Get all diagonals in a board by transposing the board and finding diagonal rows.
diagonals :: Board -> [Column]
diagonals board = diagonalsUpRight board ++ diagonalsUpLeft board

-- Get all up-right diagonals.
diagonalsUpRight :: Board -> [Column]
diagonalsUpRight [] = []
diagonalsUpRight board' = diagonalRows board' ++ diagonalsUpRight (map tail board')

-- Get all up-left diagonals.
diagonalsUpLeft :: Board -> [Column]
diagonalsUpLeft [] = []
diagonalsUpLeft board' = diagonalRows (reverseColumns board') ++ diagonalsUpLeft (map tail board')

-- Reverse the columns in a board.
reverseColumns :: Board -> Board
reverseColumns = map reverse

-- Get all the diagonal rows in a board.
diagonalRows :: Board -> [Column]
diagonalRows [] = []
diagonalRows ([] : _) = []
diagonalRows board' = head board' : diagonalRows (map tail board')


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