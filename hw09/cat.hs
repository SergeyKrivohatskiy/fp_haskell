import System.IO
import System.Environment
import Control.Monad
import Control.Exception(catch)

{-
cat принимает имена файлов и выводит их содержимое на экран.
Если в cat не передаются параметры, то она копирует stdin в stdout.
Если один из файлов не существовует, нужно вывести сообщение об ошибке и продолжить работу.
(1.5 балла)
-}

printStdin :: IO ()
printStdin = do
    hSetBuffering stdin LineBuffering
    hSetBuffering stdout LineBuffering
    forever (getLine >>= putStrLn)

main :: IO ()
main = do fileNames <- getArgs
          if null fileNames then
            printStdin
          else
            printFiles fileNames where
                printFiles [] = return ()
                printFiles (fileName: fileNames) = do
                    catch (readFile fileName) exHandler >>= putStr 
                    printFiles fileNames
                exHandler :: IOError -> IO String
                exHandler e = putStr (show e) >> return ""

