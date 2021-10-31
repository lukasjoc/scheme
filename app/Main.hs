module Main where
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad

-- Defines the grammar for a list expression
data LispVal = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | String String
  | Bool Bool

-- define a char Parser
-- that can be of any value
-- in "!#$%&|*+-/:<=>?@^_~"
symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

-- defines a string parser
-- strings are just random charssourounded by
-- double quote marks
-- ----------------------------------------------------
-- | Name  | C-escape  Description                    |
-- | ----- | --------  ------------------------------ |
-- | `BEL` | `\a`      Terminal bell                  |
-- | `BS`  | `\b`      Backspace                      |
-- | `HT`  | `\t`      Horizontal TAB                 |
-- | `LF`  | `\n`      Linefeed (newline)             |
-- | `VT`  | `\v`      Vertical TAB                   |
-- | `FF`  | `\f`      Formfeed (also: New page `NP`) |
-- | `CR`  | `\r`      Carriage return                |
-- | `ESC` | `\e`,`\\` Escape character               |
-- ----------------------------------------------------
escaped :: Parser Char
escaped = do
  char '\\'
  x <- oneOf "\\\"abtnvfr"
  return $ case x of
    '\\' -> x
    '"' -> x
    'a' -> '\a'
    'b' -> '\b'
    't' -> '\t'
    'n' -> '\n'
    'v' -> '\v'
    'f' -> '\f'
    'r' -> '\r'
  return x

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many $ escaped <|> noneOf "\"\\"
  char '"'
  return $ String x

-- defines an atom parser
-- atoms start with a letter or a symbol
-- and end with letters symbols, or digits
parseAtom :: Parser LispVal
parseAtom = do
  -- first letter is letter or symbol
  first <- letter <|> symbol

  -- rest of the letters are letters
  -- or digits or symbols
  rest <- many (letter <|> digit <|> symbol)

  -- atom is first and rest concatenated
  let atom = first:rest

  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _ -> Atom atom


-- match many digits convert to ListVal Number
parseNumber :: Parser LispVal
parseNumber = many1 digit >>=
  \num -> return ((Number . read) num)

-- the scheme parser
parseExp :: Parser LispVal
parseExp = parseAtom
  <|> parseString
  <|> parseNumber

-- define a whitespace parser to
-- allow one or more spaces
spaces :: Parser()
spaces = skipMany1 space

-- read expression with a 'lisp' parser
-- from the parsec package and then apply
-- the spaces parser before matching for symbols
readExp :: String -> String
readExp input = case parse parseExp "lisp" input of
  -- either no error
  Right val -> "Match: " ++ input

  -- error found in either
  Left err -> "No match: " ++ show err

main :: IO ()
--main = do
--  (exp:_) <- getArgs
--  putStrLn (readExp exp)

main = getArgs >>= \(exp:_)
  -> putStrLn (readExp exp)

