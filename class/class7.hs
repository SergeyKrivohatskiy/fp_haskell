import Control.Monad.State

data Pair a = Pair a a

--instance Monad Pair where
--  return a = Pair a a
--  (>>=) (Pair a1 a2) ab = case (ab a1, ab a2) of
--    (Pair r1 _, Pair _ r4) -> Pair r1 r4

while :: Monad m => m Bool -> m ()
while a = a >>= \x -> if x then while a else return ()

fac::State (Integer, Integer) ()
fac = do 
  while $do 
    (fac_, n_) <- get
    put (fac_ * n_, n_ - 1)
    return (n_ - 1 /= 0)
  return ()

fac' n = fst $ execState fac (1, n)

type Counter a = State Int a
runCounter c = runState c 0

tick :: Counter ()
tick = do
  a <- get
  put (a + 1)

fac'' 0 = return 1
fac'' n = do
  tick
  fac_ <- fac'' (n - 1)
  return (n * fac_)
