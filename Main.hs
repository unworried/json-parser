module Main where

import Parser (Parser (..), json, parse)

parseFile :: FilePath -> Parser a -> IO (Maybe a)
parseFile path parser = do
  input <- readFile path
  return $ fst <$> parse parser input

main :: IO ()
main = do
  putStr "Enter filepath to parse: "
  path <- getLine
  result <- parseFile path json
  case result of
    Nothing -> putStrLn $ "error: Failed to parse: " ++ path
    Just json -> print json
