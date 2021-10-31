module Main where
import System.Environment
import Data.Int

main :: IO()
main = do

  args <- getArgs

  let a1 = read(args !! 0) :: Int32
  let a2 = read(args !! 1) :: Int32
  let result = (+) a1 a2

  showresult :: Int32 -> Int32 -> Int32 -> String
  showresult a b c = show a ++ " + " show b ++ " = " ++ show c

  -- putStrLn(show a1 ++ "+" ++ show a2 ++ "=" ++ show result)
  putStrLn(showresult)
