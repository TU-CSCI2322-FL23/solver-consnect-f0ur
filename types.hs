{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Eta reduce" #-}

--Disk = what you play with 
--Color = what gets dropped in /Player 
--tile = indiviual holes in the board
--board 6x7 columns and rows, 42 holes in totßal  


--Column number (inputed by player when making move)
--Only storing colors in holes; not storing empty holes
-- 7 Columns by default
-- Head of column is highest color in stack
--     - when we drop a new color, it gets appended to the front of the list
--Don't need to index bc we can use  splitat
type Column = [Color]

--Possibilities of states for the holes on the board
data Color = Yellow | Red deriving (Eq, Show)

data Winner = Win Color | Stalemate deriving (Eq, Show) -- define win

--List of the states for the holes for the whole board
type Board = [Column]

--Board in game and the color of the players turn
type Game = (Board, Color)

type Move = Int

-- METH  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

makeMove :: Game -> Move -> Game
makeMove = undefined

--sees if anyone in the game has won.
gameWin :: Game -> Winner 
gameWin = undefined

--checks if the column is full
columnFull :: Column -> Bool 
columnFull givenColumn = length givenColumn == 6
--6 can change, if you want it to be 7x7, then change that value to 7

--checks if the entire board is full, indicating a draw
--maybe add condition using gameWin?
boardFull :: Board -> Bool
boardFull board = all columnFull board

validMoves :: Game -> [Move]
validMoves (board, turn) = undefined



    -- STORIES
    -- 1. Define data types or type aliases for a player, game state, move, and winner.

    -- 2. Be able to determine who has won the game state, if anyone.

    -- 3. Be able to compute the result of making a legal move in a game state.

    -- 4. Be able to compute the legal moves from a game state.

    -- 5. (If time) Be able to pretty-print a game into a string.