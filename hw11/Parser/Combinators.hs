module Combinators
    ( module Parser
    , many, many1
    , symbol, anySymbol, string, oneOf
    , digit, natural, integer
    , spaces
    , try
    , endBy, endBy1
    , sepBy, sepBy1
    , foldr1P, foldl1P
    , between, brackets, parens, braces, angles
    ) where

import Parser
import Data.Char

symbol :: Eq lex => lex -> Parser lex ()
symbol c = pure (const ()) <*> satisfy (==c)

anySymbol :: Parser lex lex
anySymbol = satisfy (const True)

digit :: Parser Char Int
digit = pure digitToInt <*> satisfy isDigit

string :: Eq lex => [lex] -> Parser lex ()
string [] = pure ()
string (x:xs) = pure (const) <*> symbol x <*> string xs

oneOf :: Eq lex => [lex] -> Parser lex lex
oneOf [] = empty
oneOf (x:xs) = satisfy (==x) <|> oneOf xs

many :: Parser lex a -> Parser lex [a]
many parserA = pure (:) <*> parserA <*> many parserA <|> pure []

many1 :: Parser lex a -> Parser lex [a]
many1 parserA = pure (:) <*> parserA <*> (many parserA <|> pure [])

natural :: Parser Char Integer
natural = pure (\digits -> fst ((readsPrec 0 digits) !! 0)) <*> many1 (satisfy isDigit)

integer :: Parser Char Integer
integer = pure (const negate) <*> symbol '-' <*> natural <|> natural

spaces :: Parser Char ()
spaces = pure (const ()) <*> many (symbol ' ')

try :: Parser lex a -> Parser lex (Maybe a)
try parserA = pure Just <*> parserA <|> pure Nothing

endBy :: Parser lex a -> Parser lex b -> Parser lex [a]
endBy parserA parserB = many (pure const <*> parserA <*> parserB)

endBy1 :: Parser lex a -> Parser lex b -> Parser lex [a]
endBy1 parserA parserB = many1 (pure const <*> parserA <*> parserB)

sepBy :: Parser lex a -> Parser lex b -> Parser lex [a]
sepBy parserA parserB = pure (:) <*> parserA <*> many (pure (\x y -> y) <*> parserB <*> parserA) <|> pure []

sepBy1 :: Parser lex a -> Parser lex b -> Parser lex [a]
sepBy1 parserA parserB = pure (:) <*> parserA <*> many (pure (\x y -> y) <*> parserB <*> parserA)

between :: Parser lex a -> Parser lex b -> Parser lex c -> Parser lex c
between parserA parserB parserC = pure (\x y z -> y) <*> parserA <*> parserC <*> parserB

brackets :: Parser Char a -> Parser Char a
brackets = between (symbol '[') (symbol ']')

parens :: Parser Char a -> Parser Char a
parens = between (symbol '(') (symbol ')')

braces :: Parser Char a -> Parser Char a
braces = between (symbol '{') (symbol '}')

angles :: Parser Char a -> Parser Char a
angles = between (symbol '<') (symbol '>')

foldr1P :: (a -> b -> a -> a) -> Parser lex a -> Parser lex b -> Parser lex a
foldr1P = undefined

foldl1P :: (a -> b -> a -> a) -> Parser lex a -> Parser lex b -> Parser lex a
foldl1P = undefined
