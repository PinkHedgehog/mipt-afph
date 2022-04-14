module ReadLine (readline) where

import Control.Exception
import Control.Monad

readline :: String -> IO (Maybe String)
readline message = do
    putStrLn message
    t <- try getLine :: IO (Either SomeException String)
    case t of
        Right a -> return $ Just a
        _       -> return Nothing :: IO (Maybe String)
