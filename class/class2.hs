numbersCount :: Int -> Int
numbersCount = length . show

repl :: Int -> a -> [a]
repl = \i -> take i . repeat

--import Data.List

--f :: String -> String
--f s = unlines (map unwords (transpose (map words (lines s))))

mapn :: (Int -> a -> b) -> [a] -> [b]
mapn = mapn_rec 0
    where mapn_rec _ f [] = []
          mapn_rec n f (a:xp) = (f n a) : (mapn_rec (n + 1) f xp)