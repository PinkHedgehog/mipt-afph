import Control.Concurrent (forkIO)
import Control.Exception (handle, SomeException)
import Control.Monad (forever)
import qualified Data.ByteString.Lazy as L
import ReadLine
import Codec.Compression.GZip
import GHC.Base (undefined)

callSomeValve :: IO ()
callSomeValve = undefined


main = do
    maybeLine <- readline "Enter a file to compress> "
    case maybeLine of
        Nothing -> return () -- user entered EOF
        Just "" -> return () -- treat no name as "want to quit"
        Just name -> do
            handle (print :: SomeException -> IO ()) $ do
                content <- L.readFile name
                forkIO (compressFile name content)
                --forkIO (callSomeValve)
                return ()
            main
    where compressFile path = L.writeFile (path ++ ".gz") . compress
