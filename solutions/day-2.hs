module Main (main) where

import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString                  as B
import           Data.Either                      (fromRight)
import           System.Environment               (getArgs)

data Range    = Range Int Int
type Input    = [Range]
type Solution = Int

unwrap :: Range -> [Int]
unwrap (Range a b) = enumFromTo a b

isInvalid :: Int -> Bool
isInvalid n =
  let
    cs = show n
    (cs1, cs2) = splitAt (length cs `div` 2) cs
  in cs1 == cs2

parser :: B.ByteString -> Input
parser = fromRight [] . A.parseOnly inputP
  where
    inputP = A.sepBy rangeP (A.char ',')
    rangeP = Range <$> A.decimal <* A.char '-' <*> A.decimal

-- | The function which calculates the solution for part one
solve1 :: Input -> Solution
solve1 = sum . concatMap (filter isInvalid . unwrap)

-- | The function which calculates the solution for part two
solve2 :: Input -> Solution
solve2 = error "Part 2 Not implemented"

main :: IO ()
main = do
  -- run this with cabal run -- day-x <part-number> <file-to-solution>
  -- example: cabal run -- day-3 2 "./input/day-3.example"
  -- will run part two of day three with input file ./input/day-3.example
  [part, filepath] <- getArgs
  input <- parser <$> B.readFile filepath -- use parser <$> readFile filepath if String is better
  if read @Int part == 1
    then do
      putStrLn "solution to problem 1 is:"
      print $ solve1 input
    else do
      putStrLn "solution to problem 2 is:"
      print $ solve2 input

