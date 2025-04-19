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
gameLoop 0 currentTurn board = do
    condition <- checkWinningCondition 0 currentTurn board
    if condition == "firstsWins"
        then putStrLn "A & B & C Wins!"

    else if condition == "lastWins"
        then putStrLn "Z Wins!"

    else do
        putStrLn "Draw! No move left."
        printBoard board
gameLoop remainingMoves currentTurn board = do
    condition <- checkWinningCondition remainingMoves currentTurn board
    if condition == "firstsWins"
        then putStrLn "A & B & C Wins!"

    else if condition == "lastWins"
        then putStrLn "Z Wins!"

    else if condition == "noMoveLeft"
        then putStrLn "Draw! No move left."

    else do
        updatedBoard <- instruction currentTurn board
        printBoard updatedBoard
        let nextTurn = if currentTurn == "firsts" then "last" else "firsts"
        gameLoop (remainingMoves - 1) nextTurn updatedBoard

checkWinningCondition :: Int -> String -> String -> IO String
checkWinningCondition remainingMoves currentTurn board = do
    if currentTurn == "last" 
        then do
            let index = elemIndex 'Z' board
            if index == Just 9 && (board !! 3) /= '_' && (board !! 8) /= '_' && (board !! 13) /= '_'
                then return "firstsWins"
                else return "continue"
    else do
        let maybeA = elemIndex 'A' board
        let maybeB = elemIndex 'B' board
        let maybeC = elemIndex 'C' board
        let maybeZ = elemIndex 'Z' board

        case (maybeA, maybeB, maybeC, maybeZ) of
            (Just aIndex, Just bIndex, Just cIndex, Just zIndex) -> do
                -- Use do notation to extract row values from IO
                aRow <- findRow aIndex
                bRow <- findRow bIndex
                cRow <- findRow cIndex
                zRow <- findRow zIndex

                -- Check if zRow is less than aRow, bRow, and cRow (meaning Z is ahead of the others)
                if zRow < aRow && zRow < bRow && zRow < cRow 
                    then return "lastWins"
                    else if remainingMoves == 0
                        then return "noMoveLeft"
                        else return "continue"
            _ -> return "continue"  -- If any of the letters are not found, continue


findRow :: Int -> IO Int
findRow index = do
    if index == 5 then return 1
    else if index `elem` [1, 6, 11] then return 2
    else if index `elem` [2, 7, 12] then return 3
    else if index `elem` [3, 8, 13] then return 4
    else return 5

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
replaceAt index newValue list = take index list ++ [newValue] ++ drop (index + 1) list

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