import Data.List
import Control.Applicative (liftA2)
{-
in context of main and IO:
liftA2 :: (a  -> b      -> c)     -> IO a           -> IO b      -> IO c
          solve                      read <$> getLine  getLine
          Int -> String -> String -> IO Int         -> IO String -> IO String

*liftA2* makes "regular" function *solve* take "irregular" values
*read <$> getLine*, whose type is *IO Int*, and *getLine* of type *IO String*,
instead of *Int* and *String*, respectively
-} 
solve :: Int -> String -> String
solve n input = unwords $ map (words input !!) [0,2..n-1]

main :: IO ()
main = putStrLn =<< liftA2 solve (read <$> getLine) getLine
