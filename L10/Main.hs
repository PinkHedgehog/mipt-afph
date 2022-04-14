import GrammarInit
import Parser
import Data.List

main = do
    t <- getContents
    print t
    putStrLn ""
    putStrLn ""
    print $ nub $ filter (null . fst) $ sortOn fst $ unParser parserGSyntax (t ++ "\n")
