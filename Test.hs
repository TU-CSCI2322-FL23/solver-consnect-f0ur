module Test where

import Game



-- Define several test cases for each function: games to compute valid moves, who has won, 
-- who will win, and the best move from; games that should result from making moves on those games; 
-- and strings to convert to and from games. 
-- You will need games that are only a few moves from the end. I suggest at least one each that is finished, 
-- one move, two moves, and four moves from the end.Caveat: In this context, "from the end" means "worst case 
-- number of moves, no matter how either player plays." Even if there is a way to win in one move, you might 
-- end up searching every other move first and taking those all the way to the end of the game.

type Test = IO Bool
type TestResult = (String, Bool)

assertEqual :: (Eq a, Show a) => String -> a -> a -> IO Bool
assertEqual label expected actual = do
    let result = expected == actual
    putStrLn $ label ++ " - " ++ if result then "Passed" else "Failed"
    pure result

-- GAME STATES  - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
--empty : used in other game states 
emptyGame :: Game
emptyGame = (emptyBoard, Red)
--finished 
finRedGame :: Game
finRedGame =
  let (_, player) = emptyGame
      board = [ [Red, Red, Red, Red, Empty, Empty, Empty]
              , [Yellow, Yellow, Yellow, Empty, Empty, Empty, Empty]
              , replicate 4 Empty
              , replicate 4 Empty
              , replicate 4 Empty
              , replicate 4 Empty
              , replicate 4 Empty
              ]
  in (board, player)
-- Test case: One move from the end
unoAwayGame :: Game
unoAwayGame = 
    let (_, player) = emptyGame
        board = [ [Empty, Empty, Empty, Empty, Empty, Empty, Empty]
                , [Empty, Empty, Empty, Empty, Empty, Empty, Empty]
                , [Empty, Empty, Empty, Empty, Empty, Empty, player]  
                , [Red, Red, Red, Yellow, Yellow, Red, Yellow]  
                , [Red, Yellow, Yellow, Red, Yellow, Red, Yellow]
                , [Yellow, Red, Red, Red, Yellow, Yellow, Red]
                ]
    in (board, player)
-- Test case: two move from the end
dosAwayGame :: Game
dosAwayGame =
    let (_, player) = emptyGame
        board = [ [Empty, Empty, Empty, Empty, Empty, Empty, Empty]
                , [Empty, Empty, Empty, Empty, Empty, Empty, Empty]
                , [Empty, Empty, Empty, Empty, Empty, player, Empty]  
                , [Red, Red, Red, Yellow, Yellow, Red, Yellow]  
                , [Red, Yellow, Yellow, Red, Yellow, Red, Yellow]
                , [Yellow, Red, Red, Red, Yellow, Yellow, Red]
                ]
        in (board, player)

-- Test case: Four moves from the end
cuatroAwayGame :: Game 
cuatroAwayGame =
    let (_, player) = emptyGame
        board = [ [Empty, Empty, Empty, Empty, Empty, Empty, Empty]  
                , [Empty, Empty, Empty, Empty, Empty, Empty, player]
                , [Red, Red, Red, Yellow, Yellow, Red, Yellow] 
                , [Red, Yellow, Yellow, Red, Yellow, Red, Yellow]
                , [Yellow, Red, Red, Red, Yellow, Yellow, Red]
                , [Red, Yellow, Yellow, Red, Yellow, Red, Yellow]
                ]
    in (board, player)


--tests for valid moves - - - - - - - - - - - -

-- Test case: empty board 
testValidEmpty :: Test
testValidEmpty = do
    let emptyBoard = replicate 7 []
        validMovesEmpty = validMoves (emptyBoard, Red)
    assertEqual "Empty Board: All columns are valid" [0..6] validMovesEmpty

-- Test case: full board 
testValidFull :: Test
testValidFull = do
    let fullBoard = replicate 7 (replicate 6 Red)
        validMovesFull = validMoves (fullBoard, Yellow)
    assertEqual "Full Board: No valid moves" [] validMovesFull
 
-- Test case: One move from the end
testValidUnoAway :: Test
testValidUnoAway = do
    let board = unoAwayGame
        validMovesUnoAway = validMoves board
    assertEqual "One Move from End: Verify valid moves" [0, 1, 2, 3, 4, 5, 6] validMovesUnoAway

-- Test case: two move from the end
testValidDosAway :: Test
testValidDosAway = do
    let board = dosAwayGame
        validMovesDosAway = validMoves board
    assertEqual "Two Moves from End: Verify valid moves" [0, 1, 2, 3, 4, 5, 6] validMovesDosAway

-- Test case: Four moves from the end
testValidCuatroAway :: Test
testValidCuatroAway = do
    let board = cuatroAwayGame
        validMovesCuatroAway = validMoves board
    assertEqual "Four Moves from End: Verify valid moves" [0, 1, 2, 3, 4, 5, 6] validMovesCuatroAway

--tests for who has won - - - - - - - - - - - - - - - - - - - - - - - - 

-- Test case: Finished game
testWinnerfin :: Test
testWinnerfin = do
    let finishedGame = finRedGame 
        winner = unsafeGameWin finishedGame  
    assertEqual "Finished game: Verify winner" (Win Red) winner

-- Test case: One move from the end
testWinnerOneFromEnd :: Test
testWinnerOneFromEnd = do
    let oneFromEndGame = unoAwayGame  
        winner = unsafeGameWin oneFromEndGame  
    assertEqual "One move from end: Verify winner" (Win Red) winner  

-- Test case: Two moves from the end
testWinnerTwoFromEnd :: Test
testWinnerTwoFromEnd =  do
    let twoFromEndGame = dosAwayGame 
        winner = unsafeGameWin twoFromEndGame  
    assertEqual "Two moves from end: Verify winner" (Win Red) winner  

-- Test case: Four moves from the end
testWinnerFourFromEnd :: Test
testWinnerFourFromEnd =  do
    let fourFromEndGame = cuatroAwayGame  
        winner = unsafeGameWin fourFromEndGame  
    assertEqual "Four moves from end: Verify winner" (Win Red) winner  

--tests for who will win - - - - - - - - - - - - - - - - - - - - - - - - 

-- Test case: Finished game
testWhoWillWinFinished :: Test
testWhoWillWinFinished = undefined

-- Test case: One move from the end
testWhoWillWinOneFromEnd :: Test
testWhoWillWinOneFromEnd = undefined

-- Test case: Two moves from the end
testWhoWillWinTwoFromEnd :: Test
testWhoWillWinTwoFromEnd = undefined

-- Test case: Four moves from the end
testWhoWillWinFourFromEnd :: Test
testWhoWillWinFourFromEnd = undefined

--tests for best move - - - - - - - - - - - - - - - - - - - - - - - - - 

-- Test case: Finished game
testBestMoveFinished :: Test
testBestMoveFinished = undefined

-- Test case: One move from the end
testBestMoveOneFromEnd :: Test
testBestMoveOneFromEnd = undefined

-- Test case: Two moves from the end
testBestMoveTwoFromEnd :: Test
testBestMoveTwoFromEnd = undefined

-- Test case: Four moves from the end
testBestMoveFourFromEnd :: Test
testBestMoveFourFromEnd = undefined

--tests for games resulting from moves- - - - - - - - - - - - - - - - - 

-- Test case: Finished game
testGameResultFinished :: Test
testGameResultFinished = undefined

-- Test case: One move from the end
testGameResultOneFromEnd :: Test
testGameResultOneFromEnd = undefined

-- Test case: Two moves from the end
testGameResultTwoFromEnd :: Test
testGameResultTwoFromEnd = undefined

-- Test case: Four moves from the end
testGameResultFourFromEnd :: Test
testGameResultFourFromEnd = undefined

--tests for conversion strings ????  - - - - - - - - - - - - - - - - - - 

-- Test case: Convert an empty board to a string
testEmptyBoardToString :: Test
testEmptyBoardToString = undefined

-- Test case: Convert a game state to a string and back to a game state
testStringToGameConversion :: Test
testStringToGameConversion = undefined


-- running test  - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
runTest :: (String, Test) -> IO ()
runTest (label, test) = do
    result <- test
    putStrLn $ label ++ " - " ++ if result then "Passed" else "Failed"

-- Then, for example, in your list of tests:
allTests :: [(String, Test)]
allTests =
     [ ("Empty Board Test", testValidEmpty)
    , ("Full Board Test", testValidFull)
    , ("One Move from End Test", testValidUnoAway)
    , ("Two Moves from End Test", testValidDosAway)
    , ("Four Moves from End Test", testValidCuatroAway)

    , ("Full Board Test", testWinnerfin)
    , ("One Move from End Test", testWinnerOneFromEnd)
    , ("Two Moves from End Test", testWinnerTwoFromEnd)
    , ("Four Moves from End Test", testWinnerFourFromEnd)

    -- ... (other tests)
    ]

-- Run the tests
main :: IO ()
main = mapM_ runTest allTests

