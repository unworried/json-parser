module Parser (Json (..), Parser (..), json) where

import Control.Applicative
import Control.Monad (replicateM)
import Data.Char (chr, isHexDigit)
import Data.List (intercalate)
import Numeric (readHex)

-- Either Err or Success
newtype Parser a = P {parse :: String -> Maybe (a, String)}

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

-- parse one character
item :: Parser Char
item = P foo
  where
    foo (c : cs) = Just (c, cs)
    foo _ = Nothing

-- parse a character that satisfies a predicate
sat :: (Char -> Bool) -> Parser Char
sat p = do
  x <- item
  if p x then return x else empty

-- parse a digit
digit :: Parser Char
digit = sat (\x -> x `elem` ['0' .. '9'])

-- parse a specific character
char :: Char -> Parser Char
char x = sat (== x)

-- parse a specific string
string :: String -> Parser String
string = foldr (\x -> (<*>) ((:) <$> char x)) (return [])

-- parse a natural number (including 0)
nat :: Parser Integer
nat = read <$> some digit

-- parse a signed integer
int :: Parser Integer
int = do
  -- sign <- (negate <$ char '-') <|> pure id
  -- sign <$> nat
  (negate <$ char '-') <*> nat <|> nat -- TODO: Test this!

-- parse whitespace
space :: Parser ()
space = () <$ many (sat (`elem` [' ', '\n', '\r', '\t']))

-- tokenise a given parser (ignoring leading and trailing whitespace)
token :: Parser a -> Parser a
token pa = do
  space
  a <- pa
  space
  return a

-- parse a symbol, ignoring whitespace
symbol :: Char -> Parser Char
symbol xs = token $ char xs

data Json
  = JNull
  | JBool Bool
  | JNumber Double
  | JString String
  | JArray [Json]
  | JObject [(String, Json)]
  deriving (Eq)

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
    return $ JNumber $ (*) (10 ^^ expo) $ fromIntegral integer + frac
  where
    fractional' :: Parser Double
    fractional' =
      (read . ('0' :) <$> ((:) <$> char '.' <*> some digit))
        <|> return 0

    exponent' =
      do
        char 'e' <|> char 'E'
        int <|> char '+' *> nat
        -- (*) <$> ((1 <$ char '+') <|> (-1 <$ char '-') <|> return 1) <*> nat
        <|> return 0

escapeHex :: Parser Char
escapeHex = chr . fst . head . readHex <$> replicateM 4 (sat isHexDigit)

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
        <|> (string "\\u" *> escapeHex)

jString :: Parser Json
jString = JString <$> stringLiteral

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep element = (:) <$> element <*> many (sep *> element) <|> pure []

jArray :: Parser Json
jArray = do
  symbol '['
  elems <- sepBy (symbol ',') json
  symbol ']'
  return $ JArray elems

jObject :: Parser Json
jObject =
  do
    symbol '{'
    pairs <- sepBy (symbol ',') pair
    symbol '}'
    return $ JObject pairs
  where
    pair = do
      key <- stringLiteral
      symbol ':'
      val <- token json
      return (key, val)

json :: Parser Json
json = token $ jNull <|> jBool <|> jNumber <|> jString <|> jArray <|> jObject

instance Show Json where
  show JNull = "Null"
  show (JBool bool) = show bool
  show (JNumber num) = show num
  show (JString str) = show str
  show (JArray arr) = "[ " ++ intercalate ", " (fmap show arr) ++ " ]"
  show (JObject obj) = "{ " ++ intercalate ", " (fmap (\(k, v) -> k ++ ": " ++ show v) obj) ++ " }"
