import System.Random(randomRIO)

{-
Реализуйте следующую программу.
Программа загадывает число от 1 до 100, пользователь должен отгадать его.
После каждой попытки программа говорит больше ее число или меньше.
Если пользователь не отгадал за 5 попыток, то проигрыш, иначе победа.
(1.5 балла)
-}

main :: IO ()
main = do
    number <- randomRIO (1, 100)
    tryGuess number 5
    where
        tryGuess :: Int -> Int -> IO ()
        tryGuess _ 0 = putStrLn "I'm affraid, your attempts is over..."
        tryGuess number attempts = do
            numberStrGotten <- getLine
            numberGotten <- return (read numberStrGotten)
            if number ==  numberGotten
                then putStrLn "You're God damn right!"
                else do
                    if number > numberGotten
                        then putStrLn ">"
                        else putStrLn "<"
                    tryGuess number (attempts - 1)
