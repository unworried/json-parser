module Main where

import Parser (Json (..), Parser (..), json, parse)

parseFile :: FilePath -> Parser a -> IO (Maybe a)
parseFile path parser = do
  input <- readFile path
  return $ fst <$> parse parser input

main :: IO ()
main = putStrLn "Hello, Haskell!"
