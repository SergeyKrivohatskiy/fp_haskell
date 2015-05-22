import System.Environment
import Data.String

main = do arr <- getArgs
          (putStr $ unwords arr)