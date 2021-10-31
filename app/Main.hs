module Main where
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment

-- define a char Parser
-- that can be of any value
-- in "!#$%&|*+-/:<=>?@^_~"
symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

-- read expression with a 'lisp' parser
-- from the parsec package
--
readExp :: String -> String
readExp input = case parse symbol "lisp" input of
  -- either no error
  Right val -> "Found value: " ++ show val

  -- error found in either
  Left err -> "No match: " ++ show err

main :: IO ()
main = do
  (exp:_) <- getArgs
  putStrLn (readExp exp)

