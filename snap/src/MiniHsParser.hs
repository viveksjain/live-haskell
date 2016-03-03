module MiniHsParser(runMiniParser, parseTracingLine) where

import Data.Char
import Data.Maybe
import Control.Applicative
import Control.Monad

newtype MiniHsParser a = MHP { runMHP :: String -> Maybe (a, String) }

instance Monad MiniHsParser where
  return x = MHP $ \s -> Just (x, s)
  m >>= f = MHP $ \s -> case runMHP m s of
    Just (v, s') -> runMHP (f v) s'
    Nothing -> Nothing

instance Applicative MiniHsParser where
  pure = return
  (<*>) = ap

instance Functor MiniHsParser where
  fmap = liftM

instance Alternative MiniHsParser where
  empty = MHP $ const Nothing
  m1 <|> m2 = MHP $ \s -> case runMHP m1 s of
    v@(Just _) -> v
    Nothing -> runMHP m2 s

runMiniParser :: MiniHsParser a -> String -> Maybe a
runMiniParser m s = case runMHP m s of
  Just (v, _) -> Just v
  Nothing -> Nothing

expectChar :: Char -> MiniHsParser Char
expectChar c = MHP $ \s -> case s of
  (x:xs) | x == c -> Just (x, xs)
  _ -> Nothing

expectNotChar :: String -> MiniHsParser ()
expectNotChar c = MHP $ \s -> case s of
  (x:xs) | x `elem` c -> Nothing
  (_:xs) -> Just ((), xs)
  [] -> Just ((), [])

expectSpace :: MiniHsParser Char
expectSpace = MHP $ \s -> case s of
  (x:xs) | isSpace x -> Just (x, xs)
  _ -> Nothing

eatWhitespace :: MiniHsParser ()
eatWhitespace = void $ many expectSpace

expectIdentChar :: MiniHsParser Char
expectIdentChar = MHP $ \s -> case s of
  (x:xs) | isAlphaNum x || x == '_' || x == '\'' -> Just (x, xs)
  _ -> Nothing

readIdent :: MiniHsParser String
readIdent = many expectIdentChar

readEverything :: MiniHsParser String
readEverything = MHP $ \s -> Just (s, [])

readTypeDecl :: MiniHsParser ()
readTypeDecl = void $ many (expectNotChar "=")

parseTracingLine :: MiniHsParser (String, String)
parseTracingLine =
  (,) <$> (readIdent <* eatWhitespace <* readTypeDecl) <*>
  (expectChar '=' *> eatWhitespace *> readEverything)
