module ErrorParser(parseErrors, ErrorMessage(..)) where

import Data.Char
--this import is redundant or not depending on the version of ghc...
--import Control.Applicative
import Control.Monad

data ErrorMessage = ErrorMessage FilePath Int Int String deriving (Eq, Ord, Read, Show)

newtype ErrorParser a = ErrorParser { runErrorParser :: String -> Maybe (a, String) }

instance Monad ErrorParser where
  return x = ErrorParser $ \s -> Just (x, s)
  m >>= f = ErrorParser $ \s ->
    case runErrorParser m s of
      Nothing -> Nothing
      Just (v, s') -> runErrorParser (f v) s'
  fail _ = ErrorParser $ const Nothing

instance Applicative ErrorParser where
  pure = return
  (<*>) = ap

instance Functor ErrorParser where
  fmap = liftM

expectChar :: Char -> ErrorParser ()
expectChar c = ErrorParser $ \s -> case s of
  (x:xs) | x == c -> Just ((), xs)
  _ -> Nothing

eatWhitespace :: ErrorParser ()
eatWhitespace = ErrorParser $ \s -> case s of
  [] -> Just ((), [])
  (x:xs) | isSpace x -> runErrorParser eatWhitespace xs
         | otherwise -> Just ((), xs)

eatUntil :: Char -> ErrorParser String
eatUntil c = ErrorParser $ \s -> case s of
  [] -> Just ([], [])
  (x:xs) | x == c -> Just ([], xs)
         | otherwise -> case runErrorParser (eatUntil c) xs of
    Nothing -> Nothing
    Just (v, xs) -> Just (x : v, xs)

peekNextChar :: ErrorParser (Maybe Char)
peekNextChar = ErrorParser $ \s -> case s of
  [] -> Just (Nothing, [])
  (x:xs) -> Just (Just x, x:xs)

parseOneError :: ErrorParser ErrorMessage
parseOneError = do
  filename <- eatUntil ':'
  line <- fmap read (eatUntil ':')
  col <- fmap read (eatUntil ':')
  msg1 <- eatUntil '\n'
  let readLoop = do
        mc <- peekNextChar
        case mc of
          Nothing -> return []
          Just c | isSpace c -> do
            msg <- eatUntil '\n'
            rest <- readLoop
            return $ msg ++ '\n' : rest
          _ -> return []
  msgrest <- readLoop
  let fullmsg = msg1 ++ '\n' : msgrest
  return $ ErrorMessage filename line col fullmsg

parseAllErrors :: ErrorParser [ErrorMessage]
parseAllErrors = do
  eatWhitespace
  mc <- peekNextChar
  case mc of
    Nothing -> return []
    Just _ -> do
      err <- parseOneError
      rest <- parseAllErrors
      return $ err : rest

parseErrors :: String -> [ErrorMessage]
parseErrors s = case runErrorParser parseAllErrors s of
  Nothing -> []
  Just (errors, _) -> errors
