import Network
import Control.Concurrent(forkIO)
import System.IO
import Control.Monad
import System.Environment

{-
Реализуйте простой эхо-сервер.
Это программа, которая слушает определенный порт, принимает соединения, и всё, что присылает клиент, отправляет ему обратно.
(2 балла)
-}

main :: IO ()
main = do
    args <- getArgs
    case args of
        (portToListen:_) -> do
            socket <- listenOn (PortNumber (fromInteger (read portToListen)))
            forever $ do
                (handler, host, port) <- accept socket
                forkIO $ (forever (hGetLine handler >>= hPutStrLn handler))
        otherwise -> putStrLn "Usage: echo_server {port number}"
