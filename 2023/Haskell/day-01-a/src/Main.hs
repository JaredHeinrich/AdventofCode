import Data.Char (digitToInt, isDigit)
calculateValue :: [Int] -> Int
calculateValue digits = 10 * head digits + last digits

solve :: [String] -> Int
solve = sum . map (calculateValue . map digitToInt . filter isDigit)


main :: IO ()
main = do
    let filepath :: String = "../../inputs/input-01.txt"
    input :: String <- readFile filepath
    let parsed :: [String] = lines input 
    let output :: Int = solve parsed
    print output
