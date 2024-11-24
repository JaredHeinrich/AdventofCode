import Data.List (tails, isPrefixOf)
calculateValue :: [Int] -> Int
calculateValue digits = 10 * head digits + last digits

patterns :: [(String, Int)]
patterns = [
    ("1", 1), ("2", 2), ("3", 3), ("4", 4), ("5", 5), ("6", 6), ("7", 7), ("8", 8), ("9", 9),
    ("one", 1), ("two", 2), ("three", 3), ("four", 4), ("five", 5), ("six", 6), ("seven", 7), ("eight", 8), ("nine", 9)
    ]

solve :: [String] -> Int
solve = sum . map (calculateValue . numbers)
    where
    numbers :: String -> [Int] 
    numbers line = [n | string_tail <- tails line, (literal, n) <- patterns, literal `isPrefixOf` string_tail]


main :: IO ()
main = do
    let filepath :: String = "../../inputs/input-01.txt"
    input :: String <- readFile filepath
    let parsed :: [String] = lines input 
    let output :: Int = solve parsed
    print output
