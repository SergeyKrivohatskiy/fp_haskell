import System.IO
import Control.Monad

main = hSetBuffering stdin LineBuffering >> hSetBuffering stdout LineBuffering >> (forever (getLine >>= putStrLn))