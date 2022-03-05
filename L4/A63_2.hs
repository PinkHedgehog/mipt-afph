import Data.List
import Control.Applicative (liftA2)

solve :: Int -> String -> String
solve n input = unwords $ map (words input !!) [0,2..n-1]

main :: IO ()
main = do
    -- n' <- getLine
    -- let n = read n' :: Int
    -- numbers <- getLine
    putStrLn =<< liftA2 solve (read <$> getLine) getLine
