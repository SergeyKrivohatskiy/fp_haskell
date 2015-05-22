import System.IO
import Control.Monad
import System.Environment
import Data.List

main = hSetBuffering stdin LineBuffering >> 
        hSetBuffering stdout LineBuffering >> 
        getArgs >>= 
            \(x:_) -> (forever (getLine >>= \y -> if isInfixOf x y then putStrLn y else return ()))