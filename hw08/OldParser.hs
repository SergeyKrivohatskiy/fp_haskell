-- Список экспорта менять нельзя!
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
import Test.HUnit
import Data.Foldable(toList)

type Error = Either String -- Можно заменить на Maybe, если есть желание.
newtype Parser lex a = Parser { runParser :: [lex] -> Error (a, [lex]) }
parse lexemes parser = runParser parser lexemes

-- (0.5 балла)
evalParser :: Parser lex a -> [lex] -> Error a
evalParser parser lexemes = case parse lexemes parser of
    Left err -> Left err
    Right (a, _) -> Right a

-- (0.5 балла)
satisfy :: (lex -> Bool) -> Parser lex lex
satisfy p = Parser $ \lexemes -> case lexemes of 
    [] -> Left "Empty lexemes"
    (x:xs) -> if p x then Right (x, xs) else Left "Predicate returns false"

-- (0.5 балла)
eof :: Parser lex ()
eof = Parser $ \lexemes -> case lexemes of 
    [] -> Right ((), [])
    otherwise -> Left "Not eof"

instance Functor (Parser lex) where
    fmap f parser = Parser $ \lexemes -> fmap (\(a, new_lemexes) -> (f a, new_lemexes)) (runParser parser lexemes)

instance Applicative (Parser lex) where
    -- (0.5 балла)
    pure a = Parser $ \lexemes -> Right (a, lexemes)
    -- (1.5 балл)
    (<*>) parser_a_b parser_a = Parser $ \lexemes -> case (runParser parser_a_b lexemes) of
        Right (a_b, new_lemexes) ->
            fmap (\(a, new_new_lexemes) -> (a_b a, new_new_lexemes)) (runParser parser_a new_lemexes)
        Left err -> Left err

instance Alternative (Parser lex) where
    -- (0.5 балла)
    empty = Parser $ const (Left "Error")
    -- (0.5 балла)
    (<|>) parser_a_1 parser_a_2 = Parser $ \lexemes -> case runParser parser_a_1 lexemes of
        Left _ -> runParser parser_a_2 lexemes
        Right pair -> Right pair

parserTestOK :: (Eq a, Show a, Eq lex, Show lex) => Parser lex a -> [lex] -> (a, [lex]) -> Test
parserTestOK (Parser p) s r = p s ~?= pure r

parserTestFail :: (Eq a, Show a) => Parser lex a -> [lex] -> Test
parserTestFail (Parser p) s = TestCase $ assertBool "Parser should fail" $ null $ toList (p s)
