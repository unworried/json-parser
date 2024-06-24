module Parser (Json (..), Parser (..), json, parse) where

import Control.Applicative

-- Either Err or Success
newtype Parser a = P (String -> Maybe (a, String))

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap g pa = do
    a <- pa
    return $ g a

instance Applicative Parser where
  pure :: a -> Parser a
  pure a = P $ \cs -> Just (a, cs)

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  pg <*> pa = do
    g <- pg
    g <$> pa

instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  p >>= f = P $ \cs ->
    case parse p cs of
      Nothing -> Nothing
      Just (a, str') -> parse (f a) str'

instance Alternative Parser where
  empty :: Parser a
  empty = P $ const Nothing

  (<|>) :: Parser a -> Parser a -> Parser a
  p <|> q = P $ \cs ->
    case parse p cs of
      Nothing -> parse q cs
      mx -> mx

parse :: Parser a -> String -> Maybe (a, String)
parse (P p) = p

-- parse one character
item :: Parser Char
item = P foo
  where
    foo (c : cs) = Just (c, cs)
    foo _ = Nothing

sat :: (Char -> Bool) -> Parser Char
sat p = do
  x <- item
  if p x then return x else empty

digit :: Parser Char
digit = sat (\x -> x `elem` ['0' .. '9'])

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string = foldr (\x -> (<*>) ((:) <$> char x)) (return [])

nat :: Parser Integer
nat = read <$> some digit

int :: Parser Integer
int = do
  sign <- (negate <$ char '-') <|> pure id
  sign <$> nat

space :: Parser ()
-- space = () <$ many (char ' ')
space = () <$ many (sat (`elem` [' ', '\n', '\r', '\t']))

token :: Parser a -> Parser a
token pa = do
  space
  a <- pa
  space
  return a

symbol :: String -> Parser String
symbol xs = token $ string xs

data Json
  = JNull
  | JBool Bool
  | JNumber Double
  | JString String
  | JArray [Json]
  | JObject [(String, Json)]
  deriving (Show)

jNull :: Parser Json
jNull = JNull <$ string "null"

jBool :: Parser Json
jBool =
  JBool True <$ string "true"
    <|> JBool False <$ string "false"

jNumber :: Parser Json
jNumber =
  do
    integer <- token int
    frac <- fractional'
    expo <- exponent'
    let num = (*) (10 ^^ expo) $ read $ show integer ++ "." ++ show frac
    return $ JNumber num
  where
    fractional' =
      do
        char '.'
        nat
        <|> return 0

    exponent' =
      do
        char 'e' <|> char 'E'
        int
        <|> return 0

-- May not work with certain werid characters/patterns e.g. escape chars
stringLiteral :: Parser String
stringLiteral = char '"' *> many (standard <|> escaped) <* char '"'
  where
    standard = sat ((&&) <$> (/= '"') <*> (/= '\\'))
    escaped =
      ('"' <$ string "\\\"")
        <|> ('\\' <$ string "\\\\")
        <|> ('/' <$ string "\\/")
        <|> ('\b' <$ string "\\b")
        <|> ('\f' <$ string "\\f")
        <|> ('\n' <$ string "\\n")
        <|> ('\r' <$ string "\\r")
        <|> ('\t' <$ string "\\t")

jString :: Parser Json
jString = JString <$> stringLiteral

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep element = (:) <$> element <*> many (sep *> element) <|> pure []

jArray :: Parser Json
jArray = do
  symbol "["
  elems <- sepBy (symbol ",") json
  symbol "]"
  return $ JArray elems

jObject :: Parser Json
jObject =
  do
    symbol "{"
    pairs <- sepBy (symbol ",") pair
    symbol "}"
    return $ JObject pairs
  where
    pair = do
      key <- stringLiteral
      symbol ":"
      val <- token json
      return (key, val)

json :: Parser Json
json = token $ jNull <|> jBool <|> jNumber <|> jString <|> jArray <|> jObject
