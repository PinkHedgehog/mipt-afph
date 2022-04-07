import Control.Monad
import Control.Monad.Trans.State
import Data.List

statefunc :: [Int] -> State Int Int
statefunc someValues = do
    forM_ someValues $ \x -> do
        when (even x) $ modify (+x)
        unless (even x) $ modify (*x)
    get
