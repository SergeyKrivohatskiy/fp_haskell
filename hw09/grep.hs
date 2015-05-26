import System.IO
import Control.Monad
import System.Environment
import Data.List
import Control.Exception(catch)

{-
grep принимает строку и от 0 и больше имен файлов, выводит строки, в которых встречается как подстрока переданная первым параметром строчка.
Если один из файлов не существовует, нужно вывести сообщение об ошибке и продолжить работу.
(1.5 балла)
-}

main :: IO ()
main = do args <- getArgs
          if length args < 1 then
            putStr "Usage: grep {str} {file1|file2|...}"
          else do
            (str:fileNames) <- return args
            grepFiles str fileNames where
                grepFiles _ [] = do 
                    return ()
                grepFiles str (fileName: fileNames) = do
                    stringsRed <- catch (readFile fileName) exHandler
                    grepStrings str (lines stringsRed)
                    grepFiles str fileNames
                grepStrings :: String -> [String] -> IO ()
                grepStrings str [] = return ()
                grepStrings str (strRed:stringsRed) = do
                    if isInfixOf str strRed
                        then putStrLn strRed
                        else return ()
                    grepStrings str stringsRed
                    
                exHandler :: IOError -> IO String
                exHandler e = putStr (show e) >> return ""
