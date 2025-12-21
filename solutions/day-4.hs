module Main where

import           Data.Matrix
import qualified Debug.Trace        as D
import           System.Environment (getArgs)

type Input    = Matrix Char
type Solution = Int

parser :: String -> Input
parser = fromLists . lines

-- | The function which calculates the solution for part one
solve1 :: Matrix Char -> Int
solve1 m = D.trace (prettyMatrix m) 0

-- | The function which calculates the solution for part two
solve2 :: Input -> Solution
solve2 = error "Part 2 Not implemented"

main :: IO ()
main = do
  -- run this with cabal run -- day-x <part-number> <file-to-solution>
  -- example: cabal run -- day-3 2 "./input/day-3.example"
  -- will run part two of day three with input file ./input/day-3.example
  [part, filepath] <- getArgs
  input <- parser <$> readFile filepath -- use parser <$> readFile filepath if String is better
  if read @Int part == 1
    then do
      putStrLn "solution to problem 1 is:"
      print $ solve1 input
    else do
      putStrLn "solution to problem 2 is:"
      print $ solve2 input

