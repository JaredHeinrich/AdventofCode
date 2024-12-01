import Data.List (sort, transpose)
parse :: String -> [[Int]]
parse = map parseLine . lines

parseLine :: String -> [Int]
parseLine = map read . words

toTouple :: [a] -> (a, a)
toTouple [x,y] = (x,y)
toTouple _ = error "Wrong lenght of Array"

solve :: [[Int]] -> Int
solve = sum . map (calculateDistance . toTouple) . transpose . map sort . transpose 

calculateDistance :: (Int,Int) -> Int
calculateDistance (a,b) = abs $ a-b

main :: IO ()
main = do
    let filepath :: String = "../../inputs/input-01.txt"
    input :: String <- readFile filepath
    print $ solve $ parse input
