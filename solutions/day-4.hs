module Main (main) where

import qualified Data.Matrix        as M
import           Data.Maybe         (catMaybes)
import           System.Environment (getArgs)

type Input    = M.Matrix Char
type Solution = Int

(!?) :: M.Matrix a -> (Int, Int) -> Maybe a
{-# INLINE (!?) #-}
m !? (i, j) = M.safeGet i j m

getAdjacents :: Int -> Int -> M.Matrix a -> [a]
getAdjacents i j m =
  let
    deltas = [(r,c) | r <- [-1..1], c <- [-1..1], (r,c) /= (0,0)]
    cells = foldr (\(dr, dc) -> ((i+dr,j+dc):)) [] deltas
  in
    catMaybes $ foldr (\c -> ((m !? c):)) [] cells

adjacentMatrix :: M.Matrix Char -> M.Matrix [Char]
adjacentMatrix m = M.matrix (M.nrows m) (M.ncols m) go
  where go (i, j) = getAdjacents i j m

parser :: String -> Input
parser = M.fromLists . lines

-- | The function which calculates the solution for part one
solve1 :: M.Matrix Char -> Int
solve1 m =
  let
    counts = fmap (length . filter (== '@')) (adjacentMatrix m)
    marks (i, j) =
      if (m M.! (i,j)) == '@' && (counts M.! (i, j)) < 4
        then 'x'
        else m M.! (i,j)
    chars = M.matrix (M.nrows m) (M.ncols m) marks
  in
    foldr (\c i -> if c == 'x' then i + 1 else i) 0 chars

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

