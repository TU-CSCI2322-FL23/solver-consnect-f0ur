import types
{-this where our game will be created. -}


makeMove :: Board -> Color -> Column -> Board
makeMove = undefined

--sees if anyone in the game has won.
gameWin :: Board -> Winner 
gameWin = undefined

--checks if the column is full
columnFull :: Column -> Bool 
columnFUll givenColumn = if length $ givenColumn == 6 then True else False
--6 can change, if you want it to be 7x7, then change that value to 7