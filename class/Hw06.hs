module Hw06 where

import qualified Data.Map as M
import Prelude hiding (lookup)
import Test.HUnit

------------------------------------------------------------------------------
-- 1. Реализуйте функции для работы с комплекснми числами.

data Complex = Complex { real :: Double, im :: Double } deriving (Show, Eq)

fromDouble :: Double -> Complex
fromDouble d = Complex { real = d, im = 0 }

-- Мнимая единица
i :: Complex
i = Complex { real = 0, im = 1 }

infixl 6 +., -.
(+.) :: Complex -> Complex -> Complex
(+.) (Complex a1 b1) (Complex a2 b2) = Complex (a1 + a2) (b1 + b2)

(-.) :: Complex -> Complex -> Complex
(-.) (Complex a1 b1) (Complex a2 b2) = Complex (a1 - a2) (b1 - b2)

infixl 7 *., /.
(*.) :: Complex -> Complex -> Complex
(*.) (Complex a1 b1) (Complex a2 b2) = Complex (a1 * a2 - b1 * b2) (a1 * b2 + a2 * b1)

(/.) :: Complex -> Complex -> Complex
(/.) (Complex a1 b1) (Complex a2 0) = Complex (a1 / a2) (b1 / a2)
(/.) a b = (a *. (conj b)) /. (fromDouble (sqr_norm b))
  where sqr_norm (Complex a b) = a * a + b * b

conj :: Complex -> Complex
conj (Complex a b) = Complex a (-b)

-- tests

testsComplex =
    [ i *. i ~?= fromDouble (-1)
    , fromDouble 3 +. i ~?= Complex 3 1
    , fromDouble 3 *. i ~?= Complex 0 3
    , (fromDouble 3 +. fromDouble 4 *. i) *. (fromDouble 4 +. fromDouble 3 *. i) ~?= fromDouble 25 *. i
    , conj (fromDouble 3 +. fromDouble 4 *. i) ~?= fromDouble 3 -. fromDouble 4 *. i
    , fromDouble 2 /. (fromDouble 1 +. i) ~?= fromDouble 1 -. i
    ]

------------------------------------------------------------------------------
-- 2

data Tree a = Node { value :: a, children :: [Tree a] }

-- (a) Возвращает высоту дерева
height :: Tree a -> Int
height (Node v children) = 1 + (max_height children)
  where
    max_height [] = 0 
    max_height (x:xs) = max (height x) (max_height xs)

-- (b) Возвращает среднее арифметическое значений во всех узлах дерева
-- Необходимо вычислить эту функцию, выполнив один проход по дереву
avg :: Tree Int -> Int
avg t = x `div` y
    where (x, y) = avgRec t
          avgRec (Node v children) = (v + s, 1 + c) where (s, c) = avgRecChildrens children
          avgRecChildrens [] = (0, 0)
          avgRecChildrens (x:xs) = (s1 + s2, c1 + c2) where 
            (s1, c1) = avgRec x
            (s2, c2) = avgRecChildrens xs

-- (c) Возвращает ширину дерева
-- Ширина дерева определяется следующим образом:
-- Количество вершин на определенном уровне называется шириной уровня.
-- Ширина дерева - это максимальная ширина уровня по всем уровням.
width :: Tree a -> Int
width (Node v children) = max 1 (widthRec children)
  where
    widthRec [] = 0
    widthRec arr = max (length arr) (widthRec (next_level arr))
    next_level [] = []
    next_level ((Node v children):xs) = children ++ (next_level xs)

-- tests

(tree1, tree2, tree3) =
    ( b [b [l [b []],
            l [b [],
               l [b [l [],
                     l [],
                     b []],
                  l []]]],
         b [],
         b [],
         l []]
    , b [b [b [],
            b [b [],
               b []]],
         b [b [],
            l [b [],
               b []]],
         l [b []]]
    , b [tree1, tree2]
    )
  where l = Node 500; b = Node 300

(testsHeight, testsAvg, testsWidth) = (
    [ height tree1 ~?= 6
    , height tree2 ~?= 4
    , height tree3 ~?= 7
    ],
    [ avg tree1 ~?= 393
    , avg tree2 ~?= 330
    , avg tree3 ~?= 362
    ],
    [ width tree1 ~?= 4
    , width tree2 ~?= 5
    , width tree3 ~?= 7
    ])

------------------------------------------------------------------------------
-- 3

data Value = I Int | B Bool deriving (Eq, Show)
data BinOp = Plus | Mul | Minus | Less | Greater | Equals deriving Show
data UnOp = Neg | Not
data Expr = BinOp BinOp Expr Expr | UnOp UnOp Expr | Const Value | If Expr Expr Expr | Var String
data Statement = Assign String Expr | While Expr Statement | Compound [Statement]

infixr 0 @=
(@=) = Assign
(.+) = BinOp Plus
(.-) = BinOp Minus
(.*) = BinOp Mul
(.<) = BinOp Less
int = Const . I
bool = Const . B
neg = UnOp Neg

type Error = String

-- evalExpr m e интерпретирует выражение e, в m передается значение переменных.
-- evalExpr возвращает либо успешно вычисленный результат, либо список ошибок.
-- Ошибки бывают двух видов: необъявленная переменная и несоответствие типов.
-- Возвращается список ошибок, т.к. выражение может содержать больше одной ошибки.
evalExpr :: M.Map String Value -> Expr -> Either [Error] Value
evalExpr vars (BinOp operation l r) = case (evalExpr vars l, evalExpr vars r) of
  (Left x, Left y) -> Left (x ++ y)
  (Left x, _) -> Left x
  (_, Left x) -> Left x
  (Right (I l_int_val), Right (I r_int_val)) -> case operation of
    Plus -> Right (I (l_int_val + r_int_val))
    Mul -> Right (I (l_int_val * r_int_val))
    Minus -> Right (I (l_int_val - r_int_val))
    Less -> Right (B (l_int_val < r_int_val))
    Greater -> Right (B (l_int_val > r_int_val))
    Equals -> Right (B (l_int_val == r_int_val))
  (Right (B l_bool_val), Right (B r_bool_val)) -> case operation of
    Equals -> Right (B (l_bool_val == r_bool_val))
    otherwise -> Left ["Operation " ++ (show operation) ++ " can't be applyed to Bools"]
  otherwise -> Left ["There is no operation for different types"]

evalExpr vars (UnOp Neg operand) = case evalExpr vars operand of
  (Left x) -> Left x
  (Right (I i)) -> Right (I (-i))
  otherwise -> Left ["Invalid operand type for (Neg). Should be (I Int)"]

evalExpr vars (UnOp Not operand) = case evalExpr vars operand of
  (Left x) -> Left x
  (Right (B b)) -> Right (B (not b))
  otherwise -> Left ["Invalid operand type for (Not). Should be (B Bool)"]

evalExpr vars (Const v) = Right v

evalExpr vars (If state if_true if_false) = case (evalExpr vars state) of
  (Left x) -> Left x
  (Right (B True)) -> evalExpr vars if_true
  (Right (B False)) -> evalExpr vars if_false
  otherwise -> Left ["If statement has invalid type. Should be (B Bool)"]
evalExpr vars (Var name) | M.member name vars = Right (vars M.! name)
                         | otherwise = Left ["Variable " ++ name ++ " not found"]

-- evalStatement принимает текущее значение переменных и statement и возвращает новое значение переменных после его выполнения.
evalStatement :: M.Map String Value -> Statement -> Either [Error] (M.Map String Value)
evalStatement vars (Assign name expr) = case (evalExpr vars expr) of
  (Left x) -> Left x
  (Right value) -> Right (M.insert name value vars)
evalStatement vars (While state body) = case (evalExpr vars state) of
  (Left x) -> Left x
  (Right (B True)) -> case (evalStatement vars body) of
    (Left x) -> Left x
    (Right new_vars) -> evalStatement new_vars (While state body)
  (Right (B False)) -> Right vars
  (Right (I _)) -> Left ["While statement should have Bool type"]
evalStatement vars (Compound []) = Right vars
evalStatement vars (Compound (x:xs)) = case evalStatement vars x of
  (Left x) -> (Left x)
  (Right new_vars) -> evalStatement new_vars (Compound xs)

-- tests

max' x y = If (x .< y) y x
expr1 = Var "x" .+ int 3
expr2 = If (Var "x") (Var "y" .- int 3) (int 2)
stat1 = Compound
    [ "x" @= int 3 .+ int 4
    , "y" @= Var "x" .* int 6
    , "z" @= neg $ max' (Var "x") (Var "y")
    ]
stat2 = Compound
    [ "r" @= int 1
    , "i" @= int 0
    , While (Var "i" .< Var "n") $ Compound
        [ "i" @= Var "i" .+ int 1
        , "r" @= Var "r" .* Var "i"
        ]
    ]

testsExpr = [ errorsCount (evalExpr M.empty expr1) ~?= 1
            , evalExpr (M.fromList [("x", B True), ("y", I 5)]) expr2 ~?= Right (I 2)
            , evalExpr (M.fromList [("x", B False), ("y", B False)]) expr2 ~?= Right (I 2)
            , errorsCount (evalExpr (M.fromList [("x", B True), ("y", B False)]) expr2) ~?= 1
            , fmap (M.lookup "z") (evalStatement M.empty stat1) ~?= Right (Just $ I $ -42)
            , fmap (M.lookup "r") (evalStatement (M.fromList [("n", I 6)]) stat2) ~?= Right (Just $ I 720)
            ]
  where errorsCount = either length (const 0)

------------------------------------------------------------------------------
-- 4. Реализовать двоичное дерево поиска без балансировки.

-- Я не знаю, было ли так задумано, что Leaf это вроде NullPtr а не лист
-- но я исходил из этого
data Map k v = Leaf | Branch k v (Map k v) (Map k v) deriving (Show, Eq)

lookup :: Ord k => k -> Map k v -> Maybe v
lookup key Leaf = Nothing
lookup key (Branch k v m_left m_right) | key == k = Just v
                                       | key < k = lookup key m_left
                                       | key > k = lookup key m_right

insert :: Ord k => k -> v -> Map k v -> (Map k v, Maybe v)
insert k v Leaf = (Branch k v Leaf Leaf, Nothing)
insert key value (Branch k v m_left m_right) | key == k = ((Branch k value m_left m_right), Just v)
                                         | key < k = let (new_left, result) = insert key value m_left 
                                           in ((Branch k v new_left m_right), result)
                                         | key > k = let (new_right, result) = insert key value m_right
                                           in ((Branch k v m_left new_right), result)

-- С учетом того, что Leaf == NullPtr Maybe тут не нужно
--delete :: Ord k => k -> Map k v -> Maybe (Map k v)
delete :: Ord k => k -> Map k v -> Map k v
delete _ Leaf = Leaf
delete key (Branch k v m_left m_right) | key == k = let merge Leaf r = r
                                                        merge (Branch k v m_left m_right) r = Branch k v m_left (merge m_right r)
                                                        in merge m_left m_right
                                       | key < k = Branch k v (delete key m_left) m_right
                                       | key > k = Branch k v m_left (delete key m_right)

fromList :: Ord k => [(k, v)] -> Map k v
fromList [] = Leaf
fromList ((k, v):xs) = fst $ insert k v (fromList xs)

toList :: Map k v -> [(k, v)]
toList Leaf = []
toList (Branch k v left right) = (toList left) ++ ((k, v):(toList right))

-- tests

-- Функция работает неправильно. Она удаляет одинаковые элементы
sort :: Ord a => [a] -> [a]
sort = map fst . toList . fromList . map (\x -> (x, ()))

------------------------------------------------------------------------------
-- main

main = fmap (\_ -> ()) $ runTestTT $ test
      $  label "complex" testsComplex
      ++ label "height" testsHeight
      ++ label "avg" testsAvg
      ++ label "width" testsWidth
      ++ label "Expr" testsExpr
      ++ label "Map" -- можете сами написать тесты на каждую функцию :)
            [ sort [10,24,13,56,35,13,6,23] ~?= [6,10,13,23,24,35,56],
             delete 1 (fst  (insert 1 0 Leaf)) ~?= Leaf,
             lookup 1 (fromList [(1, 2), (2, 4), (0, 0), ((-5), (-10))]) ~?= Just 2,
             lookup 2 (fromList [(1, 2), (2, 4), (0, 0), ((-5), (-10))]) ~?= Just 4,
             lookup (-5) (fromList [(1, 2), (2, 4), (0, 0), ((-5), (-10))]) ~?= Just (-10)]
  where
    label :: String -> [Test] -> [Test]
    label l = map (\(i,t) -> TestLabel (l ++ " [" ++ show i ++ "]") t) . zip [1..]
