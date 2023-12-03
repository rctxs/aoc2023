main :: IO ()
main = do
  f1 <- readFile "input01.txt"
  f2 <- readFile "input02.txt"
  print $ taskOne f1
  -- print $ taskTwo f2

taskOne :: String -> Int
taskOne = sum . map ( helper1 . (filter ((flip elem) ['1'..'9']))) . lines

helper1 :: String -> Int
helper1 x = read (head x : ((last x) : [])) :: Int

taskTwo :: String -> [String]
taskTwo = lines 