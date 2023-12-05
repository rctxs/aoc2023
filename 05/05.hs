{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Data.List.Split

main :: IO ()
main = do
  f1 <- readFile "input01.txt"
  f2 <- readFile "input02.txt"
  print $ taskOne f1
  print $ taskTwo f2

taskOne :: String -> Int
taskOne = minimum . mapToLocations . parse

--taskTwo :: String -> Int
taskTwo = minReachableSeed . parse

mapToLocations :: ([Int], [[(Int, Int, Int)]]) -> [Int]
mapToLocations parseResult = locations
  where
    seeds = fst parseResult
    triples = snd parseResult
    locations = foldl (\s t -> map (almanacMap t) s) seeds triples

--minReachableSeed :: ([Int], [[(Int, Int, Int)]]) -> [(Int, Int)]
minReachableSeed parseResult = minSeed
  where
    seedRange (a : b : xs) = (a, a + b) : seedRange xs
    seedRange [] = []
    seedsRanges = seedRange . fst $ parseResult
    triples_reversed = reverse . snd $ parseResult
    --test = foldl (\dests ts -> concatMap (reversedAlmanacMap ts) dests) [46] triples_reversed
    --test2 = any (\t -> fst t <= 82 && 82 < snd t) seedsRanges
    minSeed =
      head
        [ n
          | n <- [0 ..],
            reachableSeed <-
              foldl
                (\dests ts -> concatMap (reversedAlmanacMap ts) dests)
                [n]
                triples_reversed,
            any
              ( \t ->
                  fst t <= reachableSeed && reachableSeed < snd t
              )
              seedsRanges
        ]

parse :: String -> ([Int], [[(Int, Int, Int)]])
parse input = (seeds, almanacTriples)
  where
    blocks = splitOn [[]] . lines $ input
    seeds = readInts . last . splitOn ": " . head . head $ blocks
    almanacTriples = map (map (toIntTriple . readInts) . tail) . tail $ blocks

    readInts = map read . splitOn " "
    toIntTriple [a, b, c] = (a, b, c)

almanacMap :: [(Int, Int, Int)] -> Int -> Int
almanacMap [] source = source
almanacMap ((dest_start, source_start, range) : xs) source =
  if source >= source_start && source < source_start + range
    then dest_start + source - source_start
    else almanacMap xs source

reversedAlmanacMap :: [(Int, Int, Int)] -> Int -> [Int]
reversedAlmanacMap triples = reversedAlmanacMap' triples False

reversedAlmanacMap' :: [(Int, Int, Int)] -> Bool -> Int -> [Int]
reversedAlmanacMap' ((dest_start, source_start, range) : xs) hasSourceMappping dest
  | dest >= dest_start && dest < dest_start + range = source_start + dest - dest_start : reversedAlmanacMap' xs hasSourceMappping' dest
  | otherwise = reversedAlmanacMap' xs hasSourceMappping' dest
  where
    hasSourceMappping' = dest >= source_start && dest < source_start + range || hasSourceMappping
reversedAlmanacMap' [] hasSourceMappping dest = [dest | not hasSourceMappping]
