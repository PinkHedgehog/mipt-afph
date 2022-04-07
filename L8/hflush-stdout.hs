import System.IO

main = do
    putStr "Enter line: "
    hFlush stdout
    t <- getLine
    putStrLn t
