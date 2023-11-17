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

--tests for valid moves - - - - - - - - - - - -

startGame :: Game
startGame = (emptyBoard, Red)

-- Test case: Finished game
testValidMovesFinished :: Test
testValidMovesFinished = do
    let game = startGame
        moves = validMoves game
        finished = null moves  -- No moves left means finished
    pure finished
    
-- Test case: One move from the end
testValidMovesOneFromEnd :: Test
testValidMovesOneFromEnd = undefined

-- Test case: Two moves from the end
testValidMovesTwoFromEnd :: Test
testValidMovesTwoFromEnd = undefined

-- Test case: Four moves from the end
testValidMovesFourFromEnd :: Test
testValidMovesFourFromEnd = undefined

--tests for who has won - - - - - - - - - - - - - - - - - - - - - - - - 

-- Test case: Finished game
testWinnerFinished :: Test
testWinnerFinished = undefined

-- Test case: One move from the end
testWinnerOneFromEnd :: Test
testWinnerOneFromEnd = undefined

-- Test case: Two moves from the end
testWinnerTwoFromEnd :: Test
testWinnerTwoFromEnd = undefined

-- Test case: Four moves from the end
testWinnerFourFromEnd :: Test
testWinnerFourFromEnd = undefined

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
    [ ("Valid Moves Finished", testValidMovesFinished)
    -- Include other tests similarly
    ]

-- Run the tests
main :: IO ()
main = mapM_ runTest allTests