module Main where
import System.Environment
import Data.Int

main :: IO()
main = do

  args <- getArgs

  let a1 = read(args !! 0) :: Int32
  let a2 = read(args !! 1) :: Int32
  let result = (+) a1 a2

  putStrLn(show a1 ++ "+" ++ show a2 ++ "=" ++ show result)
