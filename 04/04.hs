import Data.List.Split

main :: IO ()
main = do
  f1 <- readFile "input02.txt"
  f2 <- readFile "input02.txt"
  print $ taskOne f1
  print $ taskTwo f2

taskOne :: String -> Int
taskOne = sum . map (flip score 0) . map parseLine . lines

taskTwo :: String -> Int
taskTwo =  allCards . map (uncurry matching) . map parseLine . lines

matching :: [Int] -> [Int] -> Int
matching (w:inning) you = if elem w you then 1 + (matching inning you) else (matching inning you)
matching [] _ = 0

copyCards :: [Int] -> Int
copyCards (x:xs) = if x == 0
                   then 1
                   else 1 + sum [copyCards (drop n xs) | n<-[0..(x-1)]]
copyCards [] = 0

allCards :: [Int] -> Int
allCards matching = if length matching > 1
                    then copyCards matching + ((allCards . tail) matching)
                    else copyCards matching



parseLine :: String -> ([Int], [Int])
parseLine l = (winning, you)
  where
    number_str = (last . splitOn ":") l
    winning_str = (head . splitOn " |") number_str
    you_str = (last . splitOn " |" ) number_str
    winning = parseInts winning_str
    you = parseInts you_str
    parseInts [] = []
    parseInts xs = ((read . take 3) xs) : ((parseInts . drop 3) xs)

score :: ([Int], [Int]) -> Int -> Int
score (w:inning, you) old_score
  | elem w you = if old_score == 0 then score (inning, you) 1 else score (inning, you) (2*old_score)
  | otherwise = score (inning, you) old_score
score ([], _) old_score = old_score
