import Hw06(Tree(..))

data Nat = Zero | Suc Nat deriving Eq
data Z = Pos Nat | Neg Nat


to_int Zero = 0
to_int (Suc prev) = 1 + (to_int prev)

from_int :: Integer -> Nat
from_int 0 = Zero
from_int i = Suc (from_int (i - 1))

instance Eq Z where
  (==) (Pos Zero) (Neg Zero) = True
  (==) (Pos f) (Pos s) = f == s
  (==) (Neg f) (Neg s) = f == s
  (==) _ _ = False

instance Show Nat where
  show i = show (to_int i)

instance Show Z where
  show (Pos i) = show i
  show (Neg Zero) = "0"
  show (Neg i) = "-" ++ (show i)

instance Ord Nat where
  (<=) Zero Zero = True
  (<=) Zero _ = True
  (<=) (Suc i) (Suc j) = i <= j
  (<=) _ _ = False

instance Ord Z where
  (<=) (Pos Zero) (Neg Zero) = True
  (<=) (Neg _) (Pos _) = True
  (<=) (Neg i) (Neg j) = i >= j
  (<=) (Pos i) (Pos j) = i <= j
  (<=) _ _ = False

instance Num Nat where
  (+) i Zero = i
  (+) i (Suc j) = Suc (i + j)
  (*) _ Zero = Zero
  (*) i (Suc j) = i + (i * j)
  abs = id
  signum Zero = Zero
  signum _ = Suc Zero
  fromInteger = from_int
  negate _ = error "Can't do negate for Nat"

instance Num Z where
  (+) (Pos i) (Pos j) = Pos (i + j)
  (+) (Neg i) (Neg j) = Neg (i + j)
  (+) (Pos i) (Neg j) | i >= j = Pos (from_int ((to_int i) - (to_int j)))
                      | otherwise = Neg (from_int ((to_int j) - (to_int i)))
  (+) i j = j + i
  (*) (Pos i) (Pos j) = Pos (i * j)
  (*) (Neg i) (Neg j) = Pos (i * j)
  (*) (Pos i) (Neg j) = Neg (i * j)
  (*) (Neg i) (Pos j) = Neg (i * j)
  abs (Neg i) = Pos i
  abs i = i
  signum (Neg _) = Neg (Suc Zero)
  signum _ = Pos (Suc Zero)
  fromInteger i | i >= 0 = Pos (from_int i)
                | otherwise = Neg (from_int (abs i))
  negate (Pos i) = Neg i
  negate (Neg i) = Pos i

instance Read Z where
  readsPrec unused (x:xs) | x == '-' = map (\(a, b)->((Neg . from_int)a, b)) (readsPrec unused xs)
                          | otherwise = map (\(a, b)->((Pos . from_int)a, b)) (readsPrec unused (x:xs))


--instance Functor Tree where
--  fmap fun (Node a children) = Node (fun a) (child_fmap) 