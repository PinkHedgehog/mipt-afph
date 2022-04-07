import Control.Monad
import Data.List
import System.IO



starry :: Show a => a -> String
starry x = "*** " ++ show x ++ " ***"

main = do

    hSetBuffering stdout NoBuffering
    hSetBuffering stdin NoBuffering

    n <- read <$> getLine :: IO Int

    intList <- forM [1..n] $ \i -> do
        putStr (show i ++ "th number: ")
        t <- getLine
        return $ read t :: IO Int

    print . sort $ intList
    mapM_ (putStrLn . starry) $ sort intList
