module Main (main) where

import           Data.Char          (digitToInt, intToDigit)
import           Data.List          (dropWhileEnd)
import           System.Environment (getArgs)

type Input    = [String]
type Solution = Int

solve :: (String -> Int) -> Input -> Solution
solve f = sum . map f

solve1 :: Input -> Solution
solve1 = solve (picker 2)

solve2 :: Input -> Solution
solve2 = solve (picker 12)

picker :: Int -> String -> Int
picker len =
  read . map intToDigit . reverse . go len [] . reverse . map digitToInt
  where
    go 0 ans _ = ans
    go n ans digits =
      let
        (preserved, avail) = splitAt (n-1) digits
        maxDigit = maximum avail
        remain = dropWhileEnd (< maxDigit) avail
      in
        go (n-1) (maxDigit:ans) (preserved ++ init remain)

main :: IO ()
main = do
  -- run this with cabal run -- day-x <part-number> <file-to-solution>
  -- example: cabal run -- day-3 2 "./input/day-3.example"
  -- will run part two of day three with input file ./input/day-3.example
  [part, filepath] <- getArgs
  input <- lines <$> readFile filepath
  if read @Int part == 1
    then do
      putStrLn "solution to problem 1 is:"
      print $ solve1 input
    else do
      putStrLn "solution to problem 2 is:"
      print $ solve2 input

