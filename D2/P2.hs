module P2 where

import qualified P1

import qualified Data.Map as Map
import Data.Maybe



solvePartTwo :: IO ()
solvePartTwo = do
  contents <- readFile "input.txt"
  let games = lines contents


  print (getSumOfPowerSet games)



getSumOfPowerSet :: [String] -> Int
getSumOfPowerSet =  sum . map (getPowerOfGame . getMinNeededCubes . P1.splitDrawsIntoColors . P1.splitIntoBagDraws)


getMinNeededCubes :: [[String]] -> Map.Map String Int
getMinNeededCubes = foldr combineMaxCounts (Map.fromList [("red", 0), ("green", 0), ("blue", 0)]) 


combineMaxCounts :: [String] -> Map.Map String Int -> Map.Map String Int
combineMaxCounts bagdraw currmap =
  foldr updateMaxCounts currmap bagdraw


updateMaxCounts :: String -> Map.Map String Int -> Map.Map String Int
updateMaxCounts str currmap = case words str of
  [n, color] ->
    let num         = read n :: Int
        highest_val = Map.lookup color currmap
    in
      if num > fromMaybe 0 highest_val then
        Map.insert color num currmap
      else
        currmap


getPowerOfGame :: Map.Map String Int -> Int
getPowerOfGame game =
  let red   = getColorCount game "red"
      green = getColorCount game "green"
      blue  = getColorCount game "blue"
  in
    red * green * blue


getColorCount :: Map.Map String Int -> String -> Int
getColorCount game color =
  fromMaybe 0 $ Map.lookup color game