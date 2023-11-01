{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

--Disk = what you play with 
--Color = what gets dropped in /Player 
--tile = indiviual holes in the board
--board 6x7 columns and rows, 42 holes in totßal  

-- DUE WED : DATA TYPES !!!

--Column number (inputed by player when making move)
--Only storing colors in holes; not storing empty holes
-- 7 Columns by default
-- Head of column is highest color in stack
--     - when we drop a new color, it gets appended to the front of the list
type Column = [Color]

--Possibilities of states for the holes on the board
data Color = Yellow | Red deriving (Eq, Show)

--List of the states for the holes for the whole board
type Board = [Column]

--Board in game and the color of the players turn
type Game = (Board, Color)

-- Who won?
type Winner = Color


--I JUST PUSHED

