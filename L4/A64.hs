import Data.List

solve :: String -> String
solve = unwords . filter (even . read) . words

main :: IO ()
main = do
    _ <- getLine
    putStrLn . solve =<< getLine
