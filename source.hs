import Data.List (elemIndex)
import Data.Maybe (fromMaybe)

-- Main program
main :: IO ()
main = do
    putStrLn "Welcome!"
    let board = ['X','A','_','_','X','B','_','_','_','Z','X','C','_','_','X']
    printBoard board
    putStrLn "Enter the maximum number of total moves allowed:"
    input <- getLine
    let totalMoves = (read input :: Int) * 2
    putStrLn "Who starts first? Type 'last' or 'firsts':"
    firstMove <- getLine

    -- Start the game
    gameLoop totalMoves firstMove board

gameLoop :: Int -> String -> String -> IO ()
gameLoop 0 _ board = do
    putStrLn "Game Over! No moves left."
    printBoard board
gameLoop remainingMoves currentTurn board = do
    updatedBoard <- instruction currentTurn board
    printBoard updatedBoard
    let nextTurn = if currentTurn == "firsts" then "last" else "firsts"
    gameLoop (remainingMoves - 1) nextTurn updatedBoard

-- Instruction logic
instruction :: String -> [Char] -> IO String
instruction turn board = do
    if turn == "firsts"
        then do
            putStrLn "Please select one of the first three letters and a cell to move it (e.g., A 6)"
            input <- getLine
            let letter = input !! 0
            let indexStr = drop 2 input
            let index = (read indexStr :: Int) - 1

            if letter `elem` ['A', 'B', 'C']
                then do
                    let maybeCurrentIndex = elemIndex letter board
                    case maybeCurrentIndex of
                        Just currentIndex -> 
                            -- Check if the move is valid
                            if (board !! index) == '_' && isValidMove turn currentIndex index
                                then do
                                    let boardAfterMove = replaceAt index letter (replaceAt currentIndex '_' board)
                                    return boardAfterMove
                                else do
                                    putStrLn "Invalid move! Now turn passes to the opposing team."
                                    return board
                        Nothing -> do
                            putStrLn "Letter not found!"
                            return board
                else do
                    putStrLn "Invalid letter! Now turn passes to the opposing team."
                    return board
    else if turn == "last"
        then do
            putStrLn "Please select a cell for the Z:"
            input <- getLine
            let index = (read input :: Int) - 1
            let maybeCurrentIndex = elemIndex 'Z' board
            case maybeCurrentIndex of
                Just currentIndex ->
                    if (board !! index) == '_' && isValidMove turn currentIndex index
                        then do
                            let boardAfterMove = replaceAt index 'Z' (replaceAt currentIndex '_' board)
                            return boardAfterMove
                        else do
                            putStrLn "Invalid move! Now turn passes to the opposing team."
                            return board
                Nothing -> do
                    putStrLn "Letter not found!"
                    return board
    else do
        putStrLn "Invalid turn!"
        return board

-- Validates if the move is allowed based on the current index and target index
isValidMove :: String -> Int -> Int -> Bool
isValidMove turn currentIndex index =
    if turn == "firsts"
        then
            index `elem` [currentIndex - 5, currentIndex - 4, currentIndex + 1, currentIndex + 5, currentIndex + 6] &&
            index >= 0 && index < 15
    else
        index `elem` [currentIndex - 6, currentIndex - 5, currentIndex - 4, currentIndex - 1, currentIndex + 1, currentIndex + 4, currentIndex + 5, currentIndex + 6] &&
        index >= 0 && index < 15


-- Replace an element in a list at a given index
replaceAt :: Int -> a -> [a] -> [a]
replaceAt i newVal xs = take i xs ++ [newVal] ++ drop (i + 1) xs

-- Print the board (you might want to adjust this for your game grid)
printBoard :: [Char] -> IO ()
printBoard [] = return ()
printBoard board = do
    putStr (take 1 board ++ " ")
    putStr (take 1 (drop 1 board) ++ " ")
    putStr (take 1 (drop 2 board) ++ " ")
    putStr (take 1 (drop 3 board) ++ " ")
    putStr (take 1 (drop 4 board) ++ " ")
    putStrLn ""
    printBoard (drop 5 board)