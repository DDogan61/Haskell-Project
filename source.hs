main :: IO ()
main = do
    putStrLn "Welcome!"
    let board = ['X','A','_','_','X','B','_','_','_','Z','X','C','_','_','X']
    printBoard board

printBoard :: [Char] -> IO ()
printBoard [] = return ()
printBoard board = do
    putStr (take 1 board ++ " ")
    putStr (take 1 (drop 1 board) ++ " ")
    putStr (take 1 (drop 2 board) ++ " ")
    putStr (take 1 (drop 3 board) ++ " ")
    putStr (take 1 (drop 4 board) ++ " ")
    putStrLn ""
    printBoard(drop 5 board)