
import Network
import System.IO
import System.Environment

host = "localhost"
port = 32000

main :: IO ()
main = withSocketsDo $ do
    -- putStrLn "Connecting..."
    handle <- connectTo host (PortNumber port)
    -- putStrLn $ "Connecting to " ++ host ++ ":" ++ show port
    hSetBuffering handle LineBuffering
    send handle "reload"
    hClose handle


send :: Handle -> String -> IO ()
send handle msg = do
    hPutStrLn handle msg
    return ()
