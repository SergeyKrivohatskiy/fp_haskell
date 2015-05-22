{-# LANGUAGE FlexibleContexts #-}
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Error
-- Реализуйте алгоритм вывода типов.
-- (10 баллов)

infixl 2 :@
data Expr = Var String |
     Expr :@ Expr |
     Lam String Expr 
     deriving (Eq, Show)

infixr 3 :->
data Type = TVar String | 
     Type :-> Type 
     deriving (Eq, Show)

mkEquasions :: (MonadWriter [(Type, Type)] m, MonadState Int m, MonadReader [(String, Type)] m) => Expr -> m Type
mkEquasions (Var symb) = do 
    vt <- ask
    case lookup symb vt of
        Just t -> return t
        Nothing -> error "Type not found"
mkEquasions (Lam symb expr) = do
    i <- get
    put (i + 1)
    t <- (local (\vt -> ((symb, TVar (show i)): vt)) (mkEquasions expr))
    vt <- ask
    return (TVar (show i) :-> t)
mkEquasions (expr1 :@ expr2) = do
    t1 <- mkEquasions expr1
    t2 <- mkEquasions expr2
    i <- get
    put (i + 1)
    tell [(t1, t2 :-> TVar (show i))]
    return (TVar (show i))

runMkEquasions expr types = (runWriter (runStateT (runReaderT (mkEquasions expr) types) 0))

-- replace type 1 to type 2 in type 3
substitution :: Type -> Type -> Type -> Type
substitution (TVar a) b (TVar t) | a == t = b
                                 | otherwise = (TVar t)
substitution a b (t1 :-> t2) = (substitution a b t1) :-> (substitution a b t2)

--Example
--substitution (TVar "a") ((TVar "b") :-> (TVar "b")) ((TVar "c") :-> (TVar "a"))

--Return unifier for types 1 and 2
unifier :: Type -> Type -> Either String (Type -> Type)
unifier (TVar t1) t2 | (TVar t1) == t2 = Right id
                     | contains t2 t1 = Left ("Unifier do not exists. " ++ (show t2) ++ " contains TVar \"" ++ t1 ++ "\"")
                     | otherwise = Right $ substitution (TVar t1) t2 where
                        contains (TVar t) t1 = t == t1
                        contains (type1 :-> type2) t1 = (contains type1 t1) || (contains type2 t1)
unifier (t1 :-> t2) (TVar t3) = unifier (TVar t3) (t1 :-> t2)
--U(σ1→σ2,τ1→τ2) = U(U(σ2,τ2)σ1,U(σ2,τ2)τ1)◦U(σ2,τ2) 
unifier (o1 :-> o2) (t1 :-> t2) = case unifier o2 t2 of
    Right uo2t2 -> case (unifier (uo2t2 o1) (uo2t2 t1)) of 
        Right tmp -> Right (tmp . uo2t2)
        Left err -> Left err
    Left err -> Left err

getType' :: Expr -> Identity (Either String Type)
getType' expr = do
    tmp <- return $ runMkEquasions expr []
    mainType <- return $ fst $ fst $ tmp
    equasions <- return $ snd $ tmp
    return $ processEquasions mainType equasions where
        processEquasions mainType [] = Right mainType
        processEquasions mainType (x:xs) = case unifier (fst x) (snd x) of
            Right unifier' -> processEquasions (unifier' mainType) (map (\p -> (unifier' $ fst p, unifier' $ snd p)) xs)
            Left err -> Left err

inferType :: Expr -> Either String Type
inferType = runIdentity . getType'

--Examples
--inferType (Lam "x" (Var "x"))
--inferType (Lam "x" ((Var "x") :@ (Var "x")))
--inferType (Lam "x" (Lam "y" ((Var "y") :@ (Var "x"))))