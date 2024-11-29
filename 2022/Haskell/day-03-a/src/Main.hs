import Data.List (intersect)
import Data.Char (isAsciiLower, isAsciiUpper)
import Data.Maybe (listToMaybe)

type Rucksack = ([Char], [Char])

parse :: String -> [Rucksack]
parse = map splitAtHalf . lines

splitAtHalf :: String -> Rucksack
splitAtHalf line = splitAt halfLineLegth line where
    lineLegth = length line
    halfLineLegth = div lineLegth 2

solve :: [Rucksack] -> Int
solve = sum . map (maybe 0 priority . listToMaybe . uncurry intersect)

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
