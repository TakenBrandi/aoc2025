module Main (main) where

import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString                  as B
import           Data.Either                      (fromRight)
import           Data.List.Extra                  (allSame)
import           Data.List.Split                  (chunksOf)
import           System.Environment               (getArgs)

data Range    = Range Int Int
type Input    = [Range]
type Solution = Int

unwrap :: Range -> [Int]
unwrap (Range a b) = enumFromTo a b

isInvalid1 :: Int -> Bool
-- Maybe someday I'll understand how this works, but for now it's too
-- clever. I can't figure out how the result of `show` is used twice
-- once to git the split point ([Char] -> Int) and again for splitAt
-- isInvalid1 = uncurry (==) . (splitAt =<< (`div` 2) . length) . show
isInvalid1 n =
  let
    n' = show n
    (cs1, cs2) = splitAt (length n' `div` 2) n'
  in cs1 == cs2

isInvalid2 :: Int -> Bool
isInvalid2 n =
  let
    n' = show n
    sizes = [x | x <- [1..length n' `div` 2], length n' `mod` x == 0]
    chunks = [chunksOf x n' | x <- sizes]
  in any allSame chunks

parser :: B.ByteString -> Input
parser = fromRight [] . A.parseOnly inputP
  where
    inputP = A.sepBy rangeP (A.char ',')
    rangeP = Range <$> A.decimal <* A.char '-' <*> A.decimal

solve1 :: Input -> Solution
solve1 = solve isInvalid1

solve2 :: Input -> Solution
solve2 = solve isInvalid2

solve :: (Int -> Bool) -> Input -> Solution
solve p = sum . concatMap (filter p . unwrap)

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

