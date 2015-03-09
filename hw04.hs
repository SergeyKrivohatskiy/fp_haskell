-- 1. fib n вовзращает n-ое число Фибоначчи.
--    Функция должна работать за линейное вермя и определена для всех целых n.
--    Для отрицательных n значение определяется по формуле fib n = fib (n + 2) - fib (n + 1).
--    (1 балл)
fib :: Integer -> Integer
fibRec (a, b, n) | n > 0 = fibRec(a + b, a, n - 1) | otherwise = (a, b, n)

fib n = let (res, _, _) = fibRec (1, 0, n)
        in res

-- 2a. Написать функцию, возвращающую количество цифр числа.
--     Для целочисленного деления можете использовать функции div и mod.
--    (0.5 балла)
numberOfDigits :: Integer -> Integer
numberOfDigits i | i < 10 = 1 | otherwise = 1 + numberOfDigits (i `div` 10)

-- 2b. Написать функцию, возвращающую сумму цифр числа.
--    (0.5 балла)
sumOfDigits :: Integer -> Integer
sumOfDigits i | i == 0 = 0 | otherwise = (i `mod` 10) + sumOfDigits (i `div` 10)

-- 3. gcd' возвращает НОД.
--    (1 балл)
gcd' :: Integer -> Integer -> Integer
gcd' = undefined

-- 4. minp p возвращает минимальное по модулю число x такое, что p x == True. Если такого x не существует, minp не завершается.
--    (1 балл)
minp :: (Integer -> Bool) -> Integer
minpRec (p, i) | (p i) = i | otherwise = minpRec (p, i + 1)
minp p = minpRec (p, 0)

-- 5. integral f a b возвращает значение определенного интеграла функции f на отрезке [a,b].
--    Для реализации можете использовать метод трапеций.
--    (2 балла)
integral :: (Double -> Double) -> Double -> Double -> Double
integral = undefined

-- 6. Реализуйте оператор примитивной рекурсии rec, используя функцию (-), укажите тип rec.
--    (1 балл)
rec z s n | n == 0 = z | otherwise = s (rec z s (n - 1)) n

-- 7. Реализуйте факторил при помощи rec.
--    (1 балл)
facRec :: Integer -> Integer
facRec n = rec 1 (*) n

-- 8. Реализуйте факториал при помощи fix.
--    (1 балл)
facFix :: Integer -> Integer
facFix = undefined
  where fix f = f (fix f)
