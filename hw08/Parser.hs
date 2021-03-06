{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
import Control.Monad
import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Identity
import Test.HUnit
import Data.Foldable(toList)

newtype Parser lex a = Parser { runParser :: StateT [lex] (ErrorT String Identity) a } deriving 
    (Functor, Applicative, Alternative, Monad)

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

parserTestOK :: (Eq a, Show a, Eq lex, Show lex) => Parser lex a -> [lex] -> (a, [lex]) -> Test
parserTestOK parser s r = (runIdentity $ runErrorT $ runStateT (runParser parser) s) ~?= Right r

parserTestFail :: (Eq a, Show a) => Parser lex a -> [lex] -> Test
parserTestFail parser s = TestCase $ assertBool "Parser should fail" $ 
    case (runIdentity $ runErrorT $ runStateT (runParser parser) s) of
        Left _ -> True
        Right _ -> False
