module Main where
import System.Environment
import Data.Int

main :: IO()

sumAndShowResult :: Int32 -> Int32 -> String
sumAndShowResult a b = show a ++ " + " ++ show b ++ " = " ++ show ((+) a b)

main = do
  args <- getArgs

  let a1 = read(args !! 0) :: Int32
  let a2 = read(args !! 1) :: Int32

  putStrLn(sumAndShowResult a1 a2)
