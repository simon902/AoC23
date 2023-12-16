module P1 where

import Data.List
import Data.List.Split
import Data.Char


solvePartOne :: IO ()
solvePartOne = do
  contents <- readFile "input.txt"
  let games = lines contents

  print (collectPossibleGameIds $ checkAllGames games) 


stripGamePrefix :: String -> String
stripGamePrefix str = drop 2 $ dropWhile (/= ':') str


splitIntoBagDraws :: String -> [String]
splitIntoBagDraws str = splitOn "; " $ stripGamePrefix str


splitDrawsIntoColors :: [String] -> [[String]]
splitDrawsIntoColors = map (splitOn ", ")


checkGame :: [[String]] -> Bool
checkGame = all (all checkColorCorrectness)


checkColorCorrectness :: String -> Bool
checkColorCorrectness str = case words str of
  [n, "red"]   -> (read n :: Int) <= 12
  [n, "green"] -> (read n :: Int) <= 13
  [n, "blue"]  -> (read n :: Int) <= 14


checkAllGames :: [String] -> [Bool]
checkAllGames = map (checkGame . splitDrawsIntoColors . splitIntoBagDraws)


collectPossibleGameIds :: [Bool] -> Int
collectPossibleGameIds games = collectPossibleGameIds' games 1 0

collectPossibleGameIds' :: [Bool] -> Int -> Int -> Int
collectPossibleGameIds' [] _ acc              = acc
collectPossibleGameIds' (game : games) id acc =
  if game then
    collectPossibleGameIds' games (id + 1) (acc + id)
  else
    collectPossibleGameIds' games (id + 1) acc
