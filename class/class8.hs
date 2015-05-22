import Control.Monad
import Control.Applicative
--instance Monad Pair where
--  return a = Pair a a
--  (>>=) (Pair a1 a2) ab = case (ab a1, ab a2) of
--    (Pair r1 _, Pair _ r4) -> Pair r1 r4

newtype Console a = Console (IO a)

runConsole (Console io) = io

getChar' :: Console Char
getChar' = Console getChar

putChar' :: Char -> Console ()
putChar' = Console . putChar

instance Functor Console where
    fmap = undefined

instance Applicative Console where
    pure = undefined
    (<*>) = undefined

instance Monad Console where
  return = Console . return
  (Console ioA) >>= aToConsoleB = Console $ do
    a <- ioA
    runConsole (aToConsoleB a)