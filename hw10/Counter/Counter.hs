-- (0.5 балла)
module Counter
    ( Counter
    , tick
    , runCounter
    ) where
import Control.Applicative
-- Монада Counter считает количество тиков, 
-- т.е. вызовов функции tick
data Counter a = Counter Int a

-- Возвращает результат вычислений и количество тиков
runCounter :: Counter a -> (a, Int)
runCounter (Counter ticks a) = (a, ticks)

instance Functor Counter where
    fmap = undefined

instance Applicative Counter where
    pure = undefined
    (<*>) = undefined

instance Monad Counter where
    return = Counter 0
    (Counter ticks1 a) >>= aToCounter2 = 
        case aToCounter2 a of 
            (Counter ticks2 b) -> Counter (ticks1 + ticks2) b

tick :: Counter ()
tick = Counter 1 ()