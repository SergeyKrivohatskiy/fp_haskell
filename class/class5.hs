import Control.Applicative
sepBy :: Alternative f => f a -> f b -> f [a]
sepBy aparser bparser = (:) <$> aparser <*> ((bparser *> sepBy aparser bparser) <|> pure [])


type Error = String

newtype Parser lexeme a = Parser { to_parser_f :: [lexeme] -> Either Error (a, [lexeme]) }
parse lexemes parser = to_parser_f parser lexemes

instance Functor (Parser lexeme) where
  fmap f parser = Parser $ \lexemes -> fmap (\(a, new_lemexes) -> (f a, new_lemexes)) (to_parser_f parser lexemes)

instance Applicative (Parser lexeme) where
  pure a = Parser $ \lexemes -> Right (a, lexemes)
  (<*>) parser_a_b parser_a = Parser $ \lexemes -> case (to_parser_f parser_a_b lexemes) of
    Right (a_b, new_lemexes) ->
      fmap (\(a, new_new_lexemes) -> (a_b a, new_new_lexemes)) (to_parser_f parser_a new_lemexes)
    Left err -> Left err

instance Alternative (Parser lexeme) where
  empty = Parser $ const (Left "Error")
  (<|>) parser_a_1 parser_a_2 = Parser $ \lexemes -> case to_parser_f parser_a_1 lexemes of
    Left _ -> to_parser_f parser_a_2 lexemes
    Right pair -> Right pair

integer :: Parser Char Int
integer = Parser $ \lexemes -> case readsPrec 0 lexemes of
  [] -> Left "Int parse error"
  ((a, new_lexemes):_) -> Right (a, new_lexemes)

symbol :: Eq lexeme => lexeme -> Parser lexeme ()
symbol a = Parser $ \lexemes -> case lexemes of
  [] -> Left "Lexeme parse error. Empty lexemes list"
  (lexeme: new_lexemes) -> if lexeme == a then Right ((), new_lexemes) else Left "Lexeme parse error. Not equal"