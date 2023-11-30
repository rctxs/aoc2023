main :: IO ()
main = do
  f1 <- readFile "input01.txt"
  print $ taskOne f1
  print $ taskTwo f1

taskOne :: String -> String
taskOne = id

taskTwo :: String -> [String]
taskTwo = lines