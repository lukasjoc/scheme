module Main where
import System.Environment
import Data.Int

main :: IO()

sumAndShowResult :: Int32 -> Int32 -> String
sumAndShowResult a b = show a ++ " + " ++ show b ++ " = " ++ show ((+) a b)

-- get a number and convert to Int32
prompt :: String -> IO String
prompt text = do
  putStrLn text
  getLine

main = do
  putStrLn "Give me 2 numbers: "

  l1 <- prompt "First Number: "
  l2 <- prompt "Second Numer: "

  let x = read l1 :: Int32
  let y = read l2 :: Int32

  putStrLn(sumAndShowResult x y)
