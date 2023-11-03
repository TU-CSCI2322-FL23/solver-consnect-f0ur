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

data Winner = Win Color | Stalemate deriving (Eq, Show) -- define win

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

--checks if the column is full
columnFull :: Column -> Bool 
columnFull givenColumn = length givenColumn == 6

-- 4. Be able to compute the legal moves from a game state.
makeMove :: Game -> Move -> Game
makeMove (currentBoard, moveColor) x = moveColor : emptyColumn  
    -- moveColumn is the column
    where 
        (before, after) = splitAt x currentBoard

splitat :: Move -> Board -> (a, b)
splitat = _
        

--checks if the entire board is full, indicating a draw
--maybe add condition using gameWin?
boardFull :: Board -> Bool
boardFull board = all columnFull board

validMoves :: Game -> [Move]
--creates a list of moves by filtering out the non-valid moves for each of the columns in the board 
validMoves (board, turn) = filter (isValidMove (board, turn)) [0..length board - 1]

--helper function for validmoves
isValidMove :: Game -> Move -> Bool
isValidMove (board, turn) column 
    | column < 0 || column >= length board = False --out of bounds column index 
    | length column >= 6 = False --the column is already full
    | otherwise = True


--sees if anyone in the game has won. CAG  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
gameWin :: Game -> Winner 
gameWin (board, color) = 
    if horizontalWin board color || verticalWin board color || diagonalWin board color 
        then Win color 
        else Stalemate 
        
--checking 4 in row 
fourInRow :: Int -> Color -> [Color] -> Bool
fourInRow n color = any (\group -> length group >= n && all (== color) group)

--horizontal win 
horizontalWin :: Board -> Color -> Bool
horizontalWin board color = undefined
   
checkHorizontal :: Color -> Column -> Bool
checkHorizontal color column = 
    fourInRow 4 color column 
    
--vertical win 
verticalWin :: Board -> Color -> Bool
verticalWin board color = 
    horizontalWin (transpose board) color

--diagonal win 
diagonalWin :: Board -> Color -> Bool
diagonalWin board color =
    any (checkDiagonal color) (diagonals board)

checkDiagonal :: Color -> [Color] -> Bool
checkDiagonal color diagonal=
    fourInRow 4 color diagonal

-- all diagonals in a board
diagonals :: Board -> [Column]
diagonals board = diagonalRows (transpose board) ++ diagonalRows (transpose (reverseColumns board))
  where
    diagonalRows [] = []
    diagonalRows ([] : _) = []
    diagonalRows board' = head board' : diagonalRows (map tail board')
    
    reverseColumns = map reverse



    -- STORIES 
    -- 1. Define data types or type aliases for a player, game state, move, and winner.

    -- 2. Be able to determine who has won the game state, if anyone.

    -- 3. Be able to compute the result of making a legal move in a game state.

    -- 5. (If time) Be able to pretty-print a game into a string.


-- test board for debugging
testBoard :: Board
testBoard = [[Yellow, Red, Yellow, Red, Yellow, Red], [Red, Yellow, Red, Yellow, Red, Yellow], [Yellow, Red, Yellow, Red, Yellow, Red], [Red, Yellow, Red, Yellow, Red, Yellow], [Yellow, Red, Yellow, Red, Yellow, Red], [Red, Yellow, Red, Yellow, Red, Yellow], [Yellow, Red, Yellow, Red, Yellow, Red]]

-- We can't print each column vertically, so we first just get a string of all the elements in a row
printRow :: [Color] -> String
printRow [] = ""
printRow lst = foldr (\x y -> show x ++ " " ++ y) "" lst

-- Prints out a pretty-looking connect four board
printBoard :: Board -> String
printBoard [] = ""
printBoard (x:xs) = printRow x ++ "\n" ++ printBoard xs

-- print all the rows but then transpose them to be
instance Show Board where
    show :: Board -> String
    show board = printBoard (transpose board)

instance Show Color where
    show :: Color -> String
    show color = 
        if color == Yellow 
            then "Y" 
            else "R"