import Network
import Control.Concurrent(forkIO)
import System.IO
import Control.Monad
import System.Environment

{-
Реализуйте простой телнет-клиент.
Это программа, которая коннектится по указанному порту и отправляет весь ввод с stdin, а ответ выводит в stdout.
(2 балла)
-}

main :: IO ()
main = do
    args <- getArgs
    case args of
        (addr:(port:_)) -> do
                handler <- connectTo addr (PortNumber (fromInteger (read port)))
                forkIO $ forever (hGetLine handler >>= putStrLn)
                forever (getLine >>= hPutStrLn handler)
        otherwise -> putStrLn "Usage: telnet {addr} {port number}"