{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Data.List.Split

main :: IO ()
main = do
  f1 <- readFile "input01.txt"
  f2 <- readFile "input02.txt"
  print $ taskOne f1

-- print $ taskTwo f2

taskOne :: String -> Int
taskOne = product . uncurry countRaces . parse

-- taskTwo = uncurry countRace . parse'
-- zu rechenintensiv (stack overflow)
-- man kann aber auch einfach seine Kenntnisse aus einfachster Analysis bentuzen
-- und Stift, Papier und Taschenrechner zur Hand nehmen...

-- distance = d, time_covered = t', v = speed, time = t, button_down = b
-- d = t' * v = (t - b) * b = t * b - b^2
-- => 0 = b^2 -tb + d
-- Dann mit p-q Formel Nullstellen von b finden.
-- Nullstellen aufrunden und Differenz bilden

parse :: String -> ([Int], [Int])
parse input = (times, distances)
  where
    parsed = map (map read . tail . filter (/= "") . splitOn " ") . lines $ input
    times = head parsed
    distances = (!! 1) parsed

countRaces :: [Int] -> [Int] -> [Int]
countRaces [] [] = []
countRaces (t : ime) (d : istance) = countRace t d : countRaces ime istance

countRace :: Int -> Int -> Int
countRace time distance = countRace' time distance time 0

countRace' :: Int -> Int -> Int -> Int -> Int
countRace' _ _ 0 winngTimes = winngTimes
countRace' time distance holding_button winningTimes = countRace' time distance (holding_button - 1) winningTimes'
  where
    distance_covered = time_left * speed
    time_left = time - holding_button
    speed = holding_button
    winningTimes' = if distance_covered > distance then winningTimes + 1 else winningTimes

parse' :: String -> (Int, Int)
parse' input = (time, distance)
  where
    parsed = map (read . filter (`elem` "0123456789")) . lines $ input
    time = head parsed
    distance = (!! 1) parsed
