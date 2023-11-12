{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Eta reduce" #-}

import Data.List (transpose)
import Data.List.Extra (intersperse)

import Types


-- Make the board -------------------------------------------------------------------------------------------------

emptyColumn :: Column
emptyColumn = []

emptyBoard :: Board
emptyBoard = replicate 7 emptyColumn

-- Making the game -----------------------------------------------------------------------------------------------

--checks if the column is full
columnFull :: Column -> Bool
columnFull givenColumn = length givenColumn == 6


-- may need to add check for valid move
makeMove :: Game -> Move -> Game
makeMove (currentBoard, moveColor) x =
        let
            (before, inclusiveAfter) = splitAt x currentBoard
            newBoard = before ++ [moveColor : head inclusiveAfter] ++ tail inclusiveAfter
        in
            (newBoard, swapColor moveColor)

-- takes in color and returns other color
swapColor :: Color -> Color
swapColor color = if color == Red then Yellow else Red


--checks if the entire board is full, indicating a draw
boardFull :: Board -> Bool
boardFull board = all columnFull board

--creates a list of moves by filtering out the non-valid moves for each of the columns in the board 
validMoves :: Game -> [Move]
validMoves (board, turn) = filter (isValidMove (board, turn)) [0..length board - 1]

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

-- Group the colors in a column into groups of 4 to check for consecutive colors.
group4 :: Column -> [Column]
group4 [] = []
group4 column
    | length column < 4 = []  -- If there are less than 4 colors, there can't be a group of 4.
    | otherwise = take 4 column : group4 (tail column)

instance Show Color where
    show Yellow = "Y"
    show Red = "R"
    

-- Check for horizontal wins on the game board.
horizontalWin :: Board -> Color -> Bool
horizontalWin board color =
    any (checkHorizontal color) board

-- Check for a horizontal win within a single column.
checkHorizontal :: Color -> Column -> Bool
checkHorizontal color column =
    any (\group -> length group >= 4 && all (== color) group) (group4 column)

-- Check for vertical wins on the transposed game board (columns become rows).

--vertical win 
verticalWin :: Board -> Color -> Bool
verticalWin board color =
    horizontalWin (transpose board) color


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

instance Show Winner where
    show (Win color) = "Winner: " ++ [showColor color]
    show Stalemate = "Stalemate"


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
printBoard :: Board -> IO ()
printBoard board = putStrLn (unlines $ map (intersperse '|') $ reverse $ transpose $ padBoard board)

-- Prints a given column of colors in reverse. This is just so that we can rotate the board 90 degrees and everything still lines up.
colToString :: [Color] -> [Char]
colToString [] = ""
colToString lst = foldr (\x y -> showColor x : y) "" $ reverse lst

padCol :: Column -> [Char]
padCol col = colToString col ++ replicate (6 - length col) ' '

padBoard :: Board -> [[Char]]
padBoard board = map padCol board

-- Returns Y for Yellow and R for Red.
showColor :: Color -> Char
showColor Yellow = 'Y'
showColor Red = 'R'
