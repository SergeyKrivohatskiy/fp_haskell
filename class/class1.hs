dist :: Double -> Double -> Double -> Double -> Double
dist x1 y1 x2 y2 = ((x1 - x2)**2+(y1 - y2)**2)**0.5

myif :: Bool -> a -> a -> a
myif b f s | b = f | otherwise = s

mysgn :: Int -> Int
mysgn x | x > 0 = 1 | x < 0 = -1 | otherwise = 0

comp :: (b -> c) -> (a -> b) -> (a -> c)
--comp f1 f2 = \x -> f1(f2(x))
comp f1 f2 x = f1(f2(x))

(+--+) :: Int -> Int
(+--+) x = 1

fib_rec (a, b, n) | n > 0 = fib_rec(a + b, a, n - 1) | otherwise = (a, b, n)

fib n = let (res, _, _) = fib_rec (1, 0, n)
        in res

rec :: a -> (a -> Int -> a) -> Int -> a
rec z s n | n == 0 = z | otherwise = s (rec z s (n - 1)) n

fac n = rec 1 (*) n

fix f = (\x -> f (x x)) (\x -> f (x x))
