-- (0.5 балла)
import Counter

-- Эти две функции отличаются от обычных тем, что, вызывая tick, они считают сколько шагов заняло их выполнение.
filter' :: (a -> Bool) -> [a] -> Counter [a]
filter' _ [] = return []
filter' predicate (a:as) = do
    filteredAs <- filter' predicate as
    tick
    if (predicate a)
        then do
            return (a:filteredAs)
        else do
            return filteredAs


append :: [a] -> [a] -> Counter [a]
append [] arr2 = return arr2
append (a:as) arr2 = do
    tick
    appendResult <- append as arr2
    return (a:appendResult)

-- Реализуйте qsort через filter' и append
qsort :: Ord a => [a] -> Counter [a]
qsort [] = return []
qsort (a:as) = do
    left <- filter' (<= a) as
    sortedLeft <- qsort left
    right <- filter' (>a) as
    sortedRight <- qsort right
    append sortedLeft (a:sortedRight)

-- Первый вызов должен занимать большее количество тиков ~ в 2 раза
main = do
    print $ runCounter $ qsort [1..15]
    print $ runCounter $ qsort [8,4,12,2,6,10,14,1,3,5,7,9,11,13,15]
