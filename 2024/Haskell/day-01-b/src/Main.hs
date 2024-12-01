import Data.List (transpose)
parse :: String -> [[Int]]
parse = map parseLine . lines

parseLine :: String -> [Int]
parseLine = map read . words

toTouple :: [a] -> (a, a)
toTouple [x,y] = (x,y)
toTouple _ = error "Wrong lenght of Array"

solve :: [[Int]] -> Int
solve rows = sum $ map (\x -> count second x * x) first where
    (first,second) = toTouple $ transpose rows


count :: Eq a => [a] -> a -> Int
count xs x = length $ filter (x ==) xs

main :: IO ()
main = do
    let filepath :: String = "../../inputs/input-01.txt"
    input :: String <- readFile filepath
    print $ solve $ parse input
