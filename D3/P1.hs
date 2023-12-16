module P1 where


import Data.Char
import qualified Data.Set as Set
import Text.Read


solvePartOne :: IO ()
solvePartOne = do
  contents <- readFile "input.txt"
  let board = lines contents

  print $ accumulateNumbers board (getValidNumberPositions board) 0


getValidNumberPositions :: [String] -> Set.Set (Int, Int)
getValidNumberPositions board =
  foldr (\xs acc -> Set.union (Set.fromList $ getAdjacent xs) acc) Set.empty symbolCoords
  -- symbolCoords
  where
    y_max = length board
    x_max = length $ head board

    isSymbol :: Char -> Bool
    isSymbol c = not (isDigit c || c == '.')

    symbolCoords = [(x, y) | x <- [0..x_max - 1], y <- [0..y_max - 1], isSymbol (board !! y !! x)]

    getAdjacent :: (Int, Int) -> [(Int, Int)]
    getAdjacent coord =
      [(x, y) |
         x <- [x_s - 1, x_s, x_s + 1],
         0 <= x,
         x < x_max,
         y <- [y_s - 1, y_s, y_s + 1],
         0 <= y,
         y < y_max,
         isDigit $ board !! y !! x]
      where
        x_s = fst coord
        y_s = snd coord


getNumbersInLine :: String -> Set.Set (Int, Int) -> (Int, Int) -> String -> Bool -> Int
getNumbersInLine [] _ _ _ _    = 0
getNumbersInLine (x:xs) lookup pos acc valid
  | isDigit x            =
      getNumbersInLine xs lookup newpos (x:acc) valid'
  | not $ isDigit x      =
      num + getNumbersInLine xs lookup newpos [] False
  where
    num    =  if valid then
                readOrValue 0 $ reverse acc
              else 0
    valid' = Set.member pos lookup || valid
    newpos = (fst pos + 1, snd pos)


readOrValue :: Read a => a -> String -> a
readOrValue defaultVal input =
  case readMaybe input of
    (Just value) -> value
    Nothing      -> defaultVal 


accumulateNumbers :: [String] -> Set.Set (Int, Int) -> Int -> Int
accumulateNumbers [] _ _        = 0
accumulateNumbers (x:xs) lookup yCoord =
  numSum + accumulateNumbers xs lookup (yCoord + 1)
  where
    numSum = getNumbersInLine x lookup (0, yCoord) "" False
