{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Eta reduce" #-}

import Data.List (transpose)

--Disk = what you play with 
--Color = what gets dropped in /Player 
--tile = indiviual holes in the board
--board 7 columns 6 rows, 42 holes in totßal  


--Column number (inputed by player when making move)
--Only storing colors in holes; not storing empty holes
-- 7 Columns by default
-- Head of column is highest color in stack
--     - when we drop a new color, it gets appended to the front of the list
--Don't need to index bc we can use  splitat
type Column = [Color]

--Possibilities of states for the holes on the board
-- We don't derive Show because we have a custom one!
data Color = Yellow | Red deriving (Eq)

data Winner = Win Color | Stalemate deriving (Eq) -- define win

--List of the states for the holes for the whole board
type Board = [Column]

--Board in game and the color of the players turn
type Game = (Board, Color)

-- 0 to 6 (makes it easier to code; we can change it to 1 to 7 later)
type Move = Int

-- METH  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

-- Make the board (finished?) --

emptyColumn :: Column
emptyColumn = []

emptyBoard :: Board
emptyBoard = replicate 7 emptyColumn

-- Making the game --

--tested: works
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


swapColor :: Color -> Color 
swapColor color = if color == Red then Yellow else Red

--checks if the entire board is full, indicating a draw
--tested: works
boardFull :: Board -> Bool
boardFull board = all columnFull board

--tested: works 
validMoves :: Game -> [Move]
--creates a list of moves by filtering out the non-valid moves for each of the columns in the board 
validMoves (board, turn) = filter (isValidMove (board, turn)) [0..length board - 1]

--tested: works
--helper function for validmoves
isValidMove :: Game -> Move -> Bool
isValidMove (board, turn) column
    | column < 0 || column >= length board = False --out of bounds column index 
    | length (board !! column) >= 6 = False --checks if the column is full 
    | otherwise = True


--sees if anyone in the game has won. CAG  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 


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

-- Check for horizontal wins on the game board.
horizontalWin :: Board -> Color -> Bool
horizontalWin board color =
    any (\column -> checkHorizontal color column) board

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
--diagonal win 
diagonalWin :: Board -> Color -> Bool
diagonalWin board color =
    any (checkDiagonal color) (diagonals board)

-- Check for a diagonal win within a list of colors.
checkDiagonal :: Color -> [Color] -> Bool
checkDiagonal color diagonal =
    any (\group -> length group >= 4 && all (== color) group) (group4 diagonal)

-- Get all diagonals in a board by transposing the board and finding diagonal rows.
diagonals :: Board -> [Column]
diagonals board = diagonalRows (transpose board) ++ diagonalRows (transpose (reverseColumns board))
  where
    diagonalRows [] = []
    diagonalRows ([] : _) = []
    diagonalRows board' = head board' : diagonalRows (map tail board')

    reverseColumns = map reverse

instance Show Winner where
    show (Win color) = "Winner: " ++ show color
    show Stalemate = "Stalemate"


    -- STORIES 
    -- 1. Define data types or type aliases for a player, game state, move, and winner.

    -- 2. Be able to determine who has won the game state, if anyone.

    -- 3. Be able to compute the result of making a legal move in a game state.

    -- 5. (If time) Be able to pretty-print a game into a string.


-- test board for debugging
testBoard :: Board
testBoard = [[Yellow, Red, Yellow, Red, Yellow, Red], [Red, Yellow, Red, Yellow, Red, Yellow], [Yellow, Red, Yellow, Red, Yellow, Red], [Red, Yellow, Red, Yellow, Red, Yellow], [Yellow, Red, Yellow, Red, Yellow, Red], [Red, Yellow, Red, Yellow, Red, Yellow], [Yellow, Red, Yellow, Red, Yellow, Red]]

--test board for validMoves
validMovesBoard :: Board
validMovesBoard = [[Yellow, Red, Yellow, Red], [Red, Yellow, Red, Red, Yellow], [Red, Yellow, Red, Yellow, Red], [Red, Yellow, Red, Red, Yellow], [Yellow, Yellow, Red, Yellow, Red], [Red, Red, Yellow], [Yellow, Red, Yellow, Red, Yellow, Red]]

-- We can't print each column vertically, so we first just get a string of all the elements in a row
printRow :: [Color] -> String
printRow [] = ""
printRow lst = foldr (\x y -> show x ++ " " ++ y) "" lst

-- Prints out a pretty-looking connect four board
printBoard :: Board -> String
printBoard [] = ""
printBoard (x:xs) = printRow x ++ "\n" ++ printBoard xs

-- print all the rows but then transpose them to be
{-instance Show Board where
    show :: Board -> String
    show board = printBoard (transpose board)-}

instance Show Color where
    show :: Color -> String

    show color = 
        if color == Yellow 
            then "Y" 
            else "R"
