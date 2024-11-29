import Data.List (intersect)
import Data.Char (isAsciiLower, isAsciiUpper)
import Data.Maybe (listToMaybe, fromJust)

type Rucksack = [Char]
type Group = (Rucksack, Rucksack, Rucksack)

parse :: String -> [Group]
parse = map toGroup . chunksOf 3 . lines

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)


toGroup :: [Rucksack] -> Group
toGroup [x,y,z] = (x,y,z)
toGroup _ = error "Wrong number of elves in List"

groupBatch :: Group -> Char
groupBatch (first, second, third) = fromJust . listToMaybe . intersect first $ intersect second third

solve :: [Group] -> Int
solve = sum . map (priority . groupBatch)

priority :: Char -> Int
priority c
    | isAsciiLower c = fromEnum c - fromEnum 'a' + 1
    | isAsciiUpper c = fromEnum c - fromEnum 'A' + 27
    | otherwise = 0

main :: IO ()
main = do
    let filepath :: String = "../../inputs/input-03.txt"
    input :: String <- readFile filepath
    print $ solve $ parse input
