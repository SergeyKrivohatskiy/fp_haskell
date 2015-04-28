{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import Control.Applicative
import Control.Monad
import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Identity

newtype Parser lex a = Parser { runParser :: StateT [lex] (ErrorT String Identity) a } deriving 
    (Functor, Applicative, Monad)

evalParser :: Parser lex a -> [lex] -> Either String a
evalParser parser lexemes = case runIdentity $ runErrorT $ runStateT (runParser parser) lexemes of
    Left str -> Left str
    Right (v, _) -> Right v

satisfy :: (lex -> Bool) -> Parser lex lex
satisfy p = Parser $ do
    lexemes <- get
    case lexemes of
        [] -> throwError "Unexpected Eof"
        (l:ls) -> do
            if p l
                then do
                    put ls
                    return l
                else throwError "False predicate"

eof :: Parser lex ()
eof = Parser $ do
    lexemes <- get
    case lexemes of [] -> return ()
                    _ -> throwError "Not eof"