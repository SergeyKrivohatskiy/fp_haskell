{-# LANGUAGE FlexibleContexts #-}
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Error

type Symb = String

infixl 2 :@
data Expr = Var Symb |
     Expr :@ Expr |
     Lam Symb Expr 
     deriving (Eq, Show)

infixr 3 :->
data Type = TVar Symb | 
     Type :-> Type 
     deriving (Eq, Show)

mkEquasions :: (MonadWriter [(Type, Type)] m, MonadState Int m, MonadReader [(Symb, Type)] m) => Expr -> m Type
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