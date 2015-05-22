import System.IO
import Control.Monad
import System.Environment
import Data.IORef

fac i = do r <- newIORef 1
           forM_ [0.. (i - 1)] $ \i -> readIORef r >>= \cur -> writeIORef r (cur * (i + 1))
           readIORef r