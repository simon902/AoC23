module Main where

import System.IO
import Data.Char
import Data.List


main :: IO ()
main = do
  contents <- readFile "input.txt"
  print $ partOne contents
  print $ partTwo contents


partOne :: String -> Int
partOne content = sum $ map getNumFromLine $ lines content


getNumFromLine :: String -> Int
getNumFromLine line = 
  let
    first = getFirstNum line
    last = getFirstNum (reverse line)
  in
    read (first : [last]) :: Int


getFirstNum :: String -> Char 
getFirstNum []     = '0'
getFirstNum (x:xs) = 
    if isDigit x then
      x
    else
      getFirstNum xs



partTwo :: String -> Int
partTwo content = sum $ map getNumFromLine' $ lines content


getNumFromLine' :: String -> Int
getNumFromLine' line = 
  let
    first = findNumInString line [] reverse
    last = findNumInString (reverse line) [] id
  in
    read (first : [last]) :: Int

findNumInString :: String -> String -> (String -> String) -> Char
findNumInString [] _ _            = '0'
findNumInString _ acc f
  | f "one"   `isPrefixOf`  acc   = '1'
  | f "two"   `isPrefixOf`  acc   = '2'
  | f "three" `isPrefixOf`  acc   = '3'
  | f "four"  `isPrefixOf`  acc   = '4'
  | f "five"  `isPrefixOf`  acc   = '5'
  | f "six"   `isPrefixOf`  acc   = '6'
  | f "seven" `isPrefixOf`  acc   = '7'
  | f "eight" `isPrefixOf`  acc   = '8'
  | f "nine"  `isPrefixOf`  acc   = '9'
findNumInString (x:xs) acc f      = 
    if isDigit x then
      x
    else
      findNumInString xs (x :acc) f


  

