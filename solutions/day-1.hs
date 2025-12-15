module Main (main) where

import           Control.Applicative              (Alternative ((<|>)))
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString                  as B
import           Data.Either                      (fromRight)
import           Data.List                        (iterate')
import           System.Environment               (getArgs)

type Input = [Step]
type Solution = Int
data Step = Step Direction Int deriving (Show)
data Direction = L | R deriving (Eq, Show)
newtype Dial = Dial Int deriving (Eq, Ord, Show)
data StepState = SS {
  _dial     :: Dial,
  positions :: [Int]
}
type StepHandler = Step -> Dial -> StepState

mkDial :: Int -> Dial
mkDial n
  | n < 0 = mkDial (n + 100)
  | n > 99 = mkDial (n - 100)
  | otherwise = Dial n

unwrap :: Dial -> Int
unwrap (Dial n) = n

op :: Direction -> Int -> Int -> Int
op = \case
  L -> (-)
  R -> (+)

-- | Moves dial and reports end position into state
moveDial :: Step -> Dial -> StepState
moveDial (Step dir dist) (Dial pos) =
  let dial' = mkDial $ op dir pos dist
  in SS dial' [unwrap dial']

-- | Moves dial and reports all passed positions into state
moveDial' :: Step -> Dial -> StepState
moveDial' (Step dir dist) dial =
  let
    SS dial' _ = moveDial (Step dir dist) dial
    dials = take dist $ tail $ iterate' (mkDial . flip (op dir) 1 . unwrap) dial
  in SS dial' (unwrap <$> dials)

parser :: B.ByteString -> Input
parser = fromRight [] . A.parseOnly (A.sepBy stepP (A.char '\n'))
  where
    stepP = Step <$> directionP <*> A.decimal
    directionP =
          L <$ A.char 'L'
      <|> R <$ A.char 'R'

solve1 :: Input -> Solution
solve1 = solve moveDial

solve2 :: Input -> Solution
solve2 = solve moveDial'

solve :: StepHandler -> Input -> Solution
solve handler = getCount . foldl doStep (SS (mkDial 50) [])
  where
    doStep (SS dial orig) step =
      let SS dial' new = handler step dial
      in SS dial' (orig ++ new)
    getCount = length . filter (== 0) . positions

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
