module Main (main) where

import           Control.Applicative              (Alternative ((<|>)))
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString                  as B
import           Data.Either                      (fromRight)
import           GHC.List                         (foldl')
import           System.Environment               (getArgs)


type Input = [Step]
type Solution = Int
data Step = Step Direction Int deriving (Show)
data Direction = L | R deriving (Show)
newtype Dial = Dial Int deriving (Eq, Ord, Show)

mkDial :: Int -> Dial
mkDial n
  | n < 0 = mkDial (n + 100)
  | n > 99 = mkDial (n - 100)
  | otherwise = Dial n

moveDial :: Dial -> Step -> Dial
moveDial (Dial pos) (Step direction distance) =
  let
    op = case direction of
      L -> (-)
      R -> (+)
    in mkDial (pos `op` distance)

parser :: B.ByteString -> Input
parser = fromRight [] . A.parseOnly (A.sepBy stepP (A.char '\n'))
  where
    stepP = Step <$> directionP <*> A.decimal
    directionP = L <$ A.char 'L' <|> R <$ A.char 'R'

solve1 :: Input -> Solution
solve1 = snd . foldl' dostep (mkDial 50, 0)
  where
    dostep (dial, count) step =
      let dial'@(Dial pos') = moveDial dial step
      in (dial', if pos' == 0 then count + 1 else count)

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

