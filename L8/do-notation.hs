import System.IO

main = do
    hSetBuffering stdout NoBuffering
    hSetBuffering stdin NoBuffering
    putStr "Enter line: "
    t <- getLine
    putStrLn t
