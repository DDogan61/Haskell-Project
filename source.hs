import Data.List (elemIndex)
import Data.Maybe (fromMaybe)
--Doğan Doğan 150123053
--Yusuf Kurt 150123078
--Beyza Parmak 150122007
-- Main program
main :: IO ()
main = do
    putStrLn "Welcome!"
    let board = ['X','A','_','_','X','B','_','_','_','Z','X','C','_','_','X']
    printBoard board
    putStrLn "Enter the maximum number of total moves allowed:"
    input <- getLine
    let totalMoves = (read input :: Int) * 2
    if totalMoves > 0 
        then do
            putStrLn "Who starts first? Type 'last' or 'firsts':"
            firstMove <- getLine
            if firstMove `elem` ["firsts", "last"]
                then do
                    -- Start the game
                    gameLoop totalMoves firstMove board
                else
                    putStrLn "The input is neither 'last' or 'firsts'. Program terminates!"
        else
            putStrLn "The input for maximum number of total moves allowed must be greater than 0. Program terminates!"

-- Function loop that runs the game
gameLoop :: Int -> String -> String -> IO ()
gameLoop 0 currentTurn board = do --If no move left, condition checked whether ABC or Z wins. Otherwise its draw
    condition <- checkWinningCondition 0 currentTurn board
    if condition == "firstsWin"
        then putStrLn "A & B & C Wins!"
    else if condition == "lastWins"
        then putStrLn "Z Wins!"
    else putStrLn "Draw! No move left."
    printBoard board --Board printed after the result
gameLoop remainingMoves currentTurn board = do
    condition <- checkWinningCondition remainingMoves currentTurn board --First we check the winning condition
    if condition == "firstsWin"
        then do putStrLn "A & B & C Wins!"
                printBoard board
    else if condition == "lastWins"
        then do putStrLn "Z Wins!"
                printBoard board
    else do --If condition is continue
        updatedBoard <- instruction currentTurn board --Board sended to instruction function to take input and update the board
        printBoard updatedBoard
        let nextTurn = if currentTurn == "firsts" then "last" else "firsts" --Turn and remainingMoves updated and gameLoop recursively called.
        gameLoop (remainingMoves - 1) nextTurn updatedBoard

checkWinningCondition :: Int -> String -> String -> IO String
checkWinningCondition remainingMoves currentTurn board = do
    if currentTurn == "last" 
        then do
            let index = elemIndex 'Z' board
            if index == Just 9 && (board !! 3) /= '_' && (board !! 8) /= '_' && (board !! 13) /= '_'
                then return "firstsWin"
                else return "continue"
    else do
        let maybeA = elemIndex 'A' board
        let maybeB = elemIndex 'B' board
        let maybeC = elemIndex 'C' board
        let maybeZ = elemIndex 'Z' board

        case (maybeA, maybeB, maybeC, maybeZ) of
            (Just aIndex, Just bIndex, Just cIndex, Just zIndex) -> do
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


findRow :: Int -> IO Int --This function is for Z's winning condition being behind all of the opposing team
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
            input <- getLine --Take input
            let letter = input !! 0 --Find the character that moves
            let indexStr = drop 2 input --Find where that character moves
            let index = read indexStr :: Int

            if letter `elem` ['A', 'B', 'C'] --Check if the letter is A or B or C
                then do
                    let maybeCurrentIndex = elemIndex letter board --Find the index of the letter
                    case maybeCurrentIndex of
                        Just currentIndex -> 
                            -- Check if the move is valid
                            if (board !! index) == '_' && isValidMove turn currentIndex index
                                then do --If valid then move that letter to that place
                                    let boardAfterMove = replaceAt index letter (replaceAt currentIndex '_' board)
                                    return boardAfterMove
                                else do --Else nothing happens, now opponent's turn
                                    putStrLn "Invalid move! Now turn passes to the opposing team."
                                    return board
                        Nothing -> do --If letter not found then nothing happens, now opponent's turn
                            putStrLn "Letter not found!"
                            return board
                else do --If letter not found then nothing happens, now opponent's turn
                    putStrLn "Invalid letter! Now turn passes to the opposing team."
                    return board
    else if turn == "last"
        then do
            putStrLn "Please select a cell for the Z:"
            input <- getLine --Find where that character moves
            let index = read input :: Int
            let maybeCurrentIndex = elemIndex 'Z' board --Find the index of Z
            case maybeCurrentIndex of
                Just currentIndex ->
                    if (board !! index) == '_' && isValidMove turn currentIndex index
                        then do --If valid then move that letter to that place
                            let boardAfterMove = replaceAt index 'Z' (replaceAt currentIndex '_' board)
                            return boardAfterMove
                        else do --Else nothing happens, now opponent's turn
                            putStrLn "Invalid move! Now turn passes to the opposing team."
                            return board
                Nothing -> do --If letter not found then nothing happens, now opponent's turn
                    putStrLn "Letter not found!"
                    return board
    else do --If letter not found then nothing happens, now opponent's turn
        putStrLn "Invalid turn!"
        return board

-- Validates if the move is allowed based on the current index and target index
isValidMove :: String -> Int -> Int -> Bool
isValidMove turn currentIndex index = --Checks whether the index is around current index. And checkes whether index is between 1 and 15
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

-- Print the board
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