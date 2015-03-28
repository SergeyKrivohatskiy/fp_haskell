module Complex where

-- (2 балл)

data Complex = Complex { real :: Double, im :: Double } deriving Eq

instance Num Complex where
    (+) (Complex a1 b1) (Complex a2 b2) = Complex (a1 + a2) (b1 + b2)
    (*) (Complex a1 b1) (Complex a2 b2) = Complex (a1 * a2 - b1 * b2) (a1 * b2 + a2 * b1)
    fromInteger i = Complex (fromInteger i) 0
    negate (Complex i j) = (Complex (negate i) (negate j))
    abs (Complex i j) = (Complex (sqrt (i * i + j * j)) 0)
    
    signum _ = error "Complex: signum isn't defined"

instance Fractional Complex where
    (/) (Complex a1 b1) (Complex a2 0) = Complex (a1 / a2) (b1 / a2)
    (/) a b = (a * (conj b)) / (Complex (sqr_norm b) 0)
      where 
        sqr_norm (Complex a b) = a * a + b * b
        conj (Complex a b) = Complex a (-b)
    fromRational i = Complex (fromRational i) 0

-- show и read должны работать как описано в тестах в Main.hs
instance Show Complex where
    show (Complex a b) | b == 0 = show a
                       | a == 0 = (show b) ++ " * i"
                       | b > 0 = (show a) ++ " + " ++ (show b) ++ " * i"
                       | b < 0 = (show a) ++ " - " ++ (show (abs b)) ++ " * i"

starts_with str [] = True
starts_with [] (_:_) = False
starts_with (x:xs) (y:ys) = x == y && starts_with xs ys

remove str 0 = str
remove (x:xs) i = remove xs (i - 1)


instance Read Complex where
    readsPrec unused str = case readsPrec unused str of 
        [] -> []
        ((a, remaind):xs) -> if starts_with remaind " * i" 
            then [(Complex 0 a, remove remaind 4)]
            else let (b, remain_remaind) = read_b unused remaind in [(Complex a b, remain_remaind)]
        where 
            read_b unused str | starts_with str " + " = case readsPrec unused (remove str 3) of 
                                    [] -> (0, str)
                                    [(b, remaind)] -> if starts_with remaind " * i" then (b, remove remaind 4) else (0, str)
                              | starts_with str " - " = case readsPrec unused (remove str 3) of 
                                    [] -> (0, str)
                                    [(b, remaind)] -> if starts_with remaind " * i" then ((negate b), remove remaind 4) else (0, str)
                              | otherwise = (0, str)

i :: Complex
i = Complex 0 1
