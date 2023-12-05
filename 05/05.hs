{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

import Data.List.Split

main :: IO ()
main = do
  f1 <- readFile "input01.txt"
  f2 <- readFile "input02.txt"
  print $ taskOne f1
  print $ taskTwo f2

parse :: String -> ([Int], [[(Int, Int, Int)]])
parse input = (seeds, almanacTriples)
  where
    blocks = splitOn [[]] . lines $ input
    seeds = readInts . last . splitOn ": " . head . head $ blocks
    almanacTriples = map (map (toIntTriple . readInts) . tail) . tail $ blocks

    readInts = map read . splitOn " "
    toIntTriple [a, b, c] = (a, b, c)

taskOne :: String -> Int
taskOne = minimum . mapToLocations . parse

mapToLocations :: ([Int], [[(Int, Int, Int)]]) -> [Int]
mapToLocations parseResult = locations
  where
    seeds = fst parseResult
    triples = snd parseResult
    locations = foldl (\s t -> map (almanacMap t) s) seeds triples

almanacMap :: [(Int, Int, Int)] -> Int -> Int
almanacMap [] source = source
almanacMap ((dest_start, source_start, range) : xs) source =
  if source >= source_start && source < source_start + range
    then dest_start + source - source_start
    else almanacMap xs source

taskTwo :: String -> Int
taskTwo = minReachableSeed2 . parse

minReachableSeed2 :: ([Int], [[(Int, Int, Int)]]) -> Int
minReachableSeed2 parseResult = minSeed
  where
    sample_seeds (a : b : xs) = [a -1, a, a + 1, b -1, b, b + 1] ++ sample_seeds xs
    sample_seeds [] = []
    sample_triples ((dest, source, range) : xs) =
      [ dest -1,
        dest,
        dest + 1,
        dest -2 + range,
        dest -1 + range,
        dest + range,
        source -1,
        source,
        source + 1,
        source -2 + range,
        source -1 + range,
        source + range
      ]
        ++ sample_triples xs
    sample_triples [] = []
    seedRange (a : b : xs) = (a, a + b) : seedRange xs
    seedRange [] = []

    -- wir nehmen die Ränder aller Intervalle inkl. +/- 1
    seed_samples = sample_seeds . fst $ parseResult
    triples = snd parseResult
    triple_samples = sample_triples . concat $ triples

    seedsRanges = seedRange . fst $ parseResult

    -- ungültige aus den Samples rausfiltern
    samples =
      filter
        ( \seed ->
            any
              ( \t ->
                  fst t <= seed && seed < snd t
              )
              seedsRanges
        )
        $ seed_samples ++ triple_samples
    locations = foldl (\s t -> map (almanacMap t) s) samples triples
    minSeed = minimum locations

-- Lösung ab hier funktioniert vermutlich
-- ist aber nicht performant genug
minReachableSeed :: ([Int], [[(Int, Int, Int)]]) -> Int
minReachableSeed parseResult = minSeed
  where
    seedRange (a : b : xs) = (a, a + b) : seedRange xs
    seedRange [] = []
    seedsRanges = seedRange . fst $ parseResult
    triples_reversed = reverse . snd $ parseResult
    reverse_destinations = \dests ts -> concatMap (reversedAlmanacMap ts) dests
    minSeed =
      head
        [ n
          | n <- [0 ..],
            reachableSeed <-
              foldl
                reverse_destinations
                [n]
                triples_reversed,
            any
              ( \t ->
                  fst t <= reachableSeed && reachableSeed < snd t
              )
              seedsRanges
        ]

reversedAlmanacMap :: [(Int, Int, Int)] -> Int -> [Int]
reversedAlmanacMap triples = reversedAlmanacMap' triples False

reversedAlmanacMap' :: [(Int, Int, Int)] -> Bool -> Int -> [Int]
reversedAlmanacMap' ((dest_start, source_start, range) : xs) hasSourceMappping dest
  | dest >= dest_start && dest < dest_start + range = source_start + dest - dest_start : reversedAlmanacMap' xs hasSourceMappping' dest
  | otherwise = reversedAlmanacMap' xs hasSourceMappping' dest
  where
    hasSourceMappping' = dest >= source_start && dest < source_start + range || hasSourceMappping
reversedAlmanacMap' [] hasSourceMappping dest = [dest | not hasSourceMappping]
