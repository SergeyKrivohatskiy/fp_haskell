-- (2 балла)
module Eval
    ( Eval, runEval
    , Error, Store
    , update, getVar
    ) where

import qualified Data.Map as M
import Data.List
import Control.Monad
import Control.Applicative

import Expr

type Error = String
type Store = M.Map String Value

newtype Eval a = Eval { runEval :: Store -> (Maybe a, [Error], Store) }

instance Functor Eval where
    fmap f (Eval m) = Eval undefined

instance Applicative Eval where
    pure x = Eval undefined
    Eval m <*> Eval k = Eval undefined

instance Monad Eval where
    return x = Eval undefined
    Eval m >>= k = Eval undefined

instance Alternative Eval where
    empty = Eval undefined
    Eval l <|> Eval r = Eval undefined

-- MonadPlus - аналог Alternative для монад
-- mzero - вычисление, которое ничего не делает, сразу завершается неуспехом
-- mplus m1 m2 пытается выполнить m1, если тот завершился неуспехом, выполняет m2
-- Примеры использования этого класса есть в Utils.hs
instance MonadPlus Eval where
    mzero = Eval undefined
    mplus (Eval l) (Eval r) = Eval undefined

update :: String -> Value -> Eval ()
update k v = Eval undefined

getVar :: String -> Eval Value
getVar v = Eval undefined
