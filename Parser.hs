module Parser where

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
  sign <- (pure negate <* char '-') <|> pure id
  sign <$> nat

space :: Parser ()
space = () <$ many (char ' ')

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
    int <- token int
    optional (char '.')
    frac <- nat <|> return 0 -- Should not be 0 in case "1." where . present but no int after
    let num = read $ show int ++ "." ++ show frac
    return $ JNumber num

jString :: Parser Json
jString = undefined

jArray :: Parser Json
jArray = undefined

jObject :: Parser Json
jObject = undefined
