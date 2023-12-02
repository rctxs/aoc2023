import Data.List.Split

main :: IO ()
main = do
  f1 <- readFile "input01.txt"
  f2 <- readFile "input02.txt"
  print $ taskOne f1
  print $ taskTwo f2

taskOne :: String -> Int
taskOne = sum . map gameId . filter isGamePossible . map parseGame . lines  

taskTwo :: String -> Int
taskTwo = sum . map gamePower . map parseGame . lines

data ColorDraw = ColorDraw Int Int Int deriving (Show, Eq)
data Game = Game Int [ColorDraw] deriving (Show, Eq)

parseGame :: String -> Game
parseGame line = Game id colorDraws
    where
        game = splitOn ": " line
        id = (read . last . splitOn " " . head) game :: Int
        colorDraws = (map parseColorDraw . splitOn "; " . last) game
        parseColorDraw = (parseColorDraw' (ColorDraw 0 0 0)) . splitOn ", "
        parseColorDraw' (ColorDraw r g b) [] =  (ColorDraw r g b)
        parseColorDraw' (ColorDraw r g b) (x:xs) = parseColorDraw' (ColorDraw r' g' b') xs
            where
                count = (read . head . splitOn " ") x :: Int
                colorStr =  (last . splitOn " ") x
                r' = if colorStr == "red" then count else r
                g' = if colorStr == "green" then count else g
                b' = if colorStr == "blue" then count else b

gameId :: Game -> Int
gameId (Game id _) = id

isGamePossible :: Game -> Bool
isGamePossible (Game id []) = True
isGamePossible (Game id ((ColorDraw r g b):xs)) = r <= 12 && g <= 13 && b <= 14 && isGamePossible (Game id xs)

gamePower :: Game -> Int
gamePower (Game _ colorDraws) = gamePower' (ColorDraw 0 0 0) colorDraws

gamePower' :: ColorDraw -> [ColorDraw] -> Int
gamePower' (ColorDraw r g b) [] = r * g * b
gamePower' (ColorDraw r g b) ((ColorDraw r' g' b'):xs)
    = gamePower' (ColorDraw (max r r') (max g g') (max b b')) xs



