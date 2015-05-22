import Data.Char
findDigit :: String -> Maybe Int
findDigit (x: xs) | isDigit x = Just (digitToInt x)
                  | otherwise = findDigit xs
findDigit [] = Nothing 

findDigitAndIndex :: String -> Maybe (Int, Int)
findDigitAndIndex = findRec 0 
    where findRec n [] = Nothing
          findRec n (x:xs) | isDigit x = Just (digitToInt x, n)
                           | otherwise = findRec (n + 1) xs

findDigitAndIndexOrLen :: String -> Either (Int, Int) Int
findDigitAndIndexOrLen = findRec 0 
    where findRec n [] = Right n
          findRec n (x:xs) | isDigit x = Left (digitToInt x, n)
                           | otherwise = findRec (n + 1) xs


data Result = DigitAndIndex Int Int | LetterAndIndex Char Int | Len Int deriving Show

findDigitAndIndexOrLetterAndIndexOrLen = findRec 0 
    where findRec n [] = Len n
          findRec n (x:xs) | isDigit x = DigitAndIndex (digitToInt x) n
                           | isLetter x = LetterAndIndex x n
                           | otherwise = findRec (n + 1) xs

printResult :: Result -> String
printResult (DigitAndIndex a b) = "Found digit " ++ show a ++ " at index " ++ show b
printResult (LetterAndIndex a b) = "Found letter " ++ show a ++ " at index " ++ show b
printResult (Len n) = "Nothing found. Len = " ++ show n


data Tree a = L a | B a (Tree a) (Tree a) deriving Show

height :: Tree a -> Int
height (L a) = 1
height (B _ t1 t2) = 1 + max (height t1) (height t2)

avg :: Integral a => Tree a -> Double
avg t = (fromIntegral x) / y
    where (x, y) = avgRec t
          avgRec (L a) = (a, 1.0)
          avgRec (B a t1 t2) = (a + t11 + t21, 1.0 + t12 + t22) where (t11, t12) = avgRec t1
                                                                      (t21, t22) = avgRec t2
data Expr = Mul Expr Expr | Const Value | Plus Expr Expr deriving Show

eval :: Expr -> Int
eval (Const a) = a
eval (Plus a b) = (eval a) + (eval b)
eval (Mul a b) = (eval a) * (eval b)