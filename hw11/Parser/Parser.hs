{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- 1 балл
module Parser
    ( Parser
    , pure, (<$>), (<$), (<*>), (<*), (*>)
    , empty, (<|>)
    , satisfy, eof
    , evalParser
    , parserTestOK
    , parserTestFail
    ) where

import Control.Applicative
import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Identity
import Test.HUnit((~?=),Test(TestCase),assertBool)

-- Реализуйте Parser, используя трансформеры монад.
newtype Parser lex a = Parser { runParser' :: StateT [lex] (ErrorT String Identity) a } 
    deriving (Functor, Applicative, Alternative, Monad, MonadPlus)

runParser :: Parser lex a -> [lex] -> Either String (a, [lex])
runParser parser lexemes = runIdentity $ runErrorT $ runStateT (runParser' parser) lexemes

evalParser :: Parser lex a -> [lex] -> Either String a
evalParser parser lexemes = case runParser parser lexemes of
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

parserTestOK :: (Eq a, Show a, Eq lex, Show lex) => Parser lex a -> [lex] -> (a, [lex]) -> Test
parserTestOK p s r = runParser p s ~?= Right r

parserTestFail :: (Eq a, Show a) => Parser lex a -> [lex] -> Test
parserTestFail p s = TestCase $ assertBool "Parser should fail" $ either (const True) (const False) (runParser p s)
