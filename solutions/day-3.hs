module Main (main) where

import           System.Environment (getArgs)

type Input    = [String]
type Solution = Int

solve1 :: Input -> Solution
solve1 banks = sum maxes
    where
      maxes = maximum <$> [ maker line | line <- banks]
      maker []     = []
      maker (x:ys) = [read [x, y] | y <- ys] ++ maker ys

solve2 :: Input -> Solution
solve2 = error "Part 2 Not implemented"

main :: IO ()
main = do
  -- run this with cabal run -- day-x <part-number> <file-to-solution>
  -- example: cabal run -- day-3 2 "./input/day-3.example"
  -- will run part two of day three with input file ./input/day-3.example
  [part, filepath] <- getArgs
  input <- lines <$> readFile filepath -- use parser <$> readFile filepath if String is better
  if read @Int part == 1
    then do
      putStrLn "solution to problem 1 is:"
      print $ solve1 input
    else do
      putStrLn "solution to problem 2 is:"
      print $ solve2 input

