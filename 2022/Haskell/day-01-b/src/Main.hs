import Data.List (sortBy)
import qualified Data.Ord
parse :: String -> [[Int]]
parse = map (map read) . splitAtEmptyLine . lines

splitAtEmptyLine :: [String] -> [[String]]
splitAtEmptyLine [] = []
splitAtEmptyLine xs = first : splitAtEmptyLine (drop 1 rest)
    where
    (first, rest) = break null xs

solve :: [[Int]] -> Int
solve = sum . take 3 . sortBy (Data.Ord.comparing Data.Ord.Down) . map sum

main :: IO ()
main = do
    let filepath :: String = "../../inputs/input-01.txt"
    input :: String <- readFile filepath
    let parsed :: [[Int]] = parse input
    let solution :: Int = solve parsed
    print solution
