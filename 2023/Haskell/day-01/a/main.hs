import Data.Char (digitToInt, isDigit)
import Text.Read (Lexeme(String))
calculateValue :: [Int] -> Int
calculateValue digits = 10 * head digits + last digits

solve :: [String] -> Int
solve = sum . map (calculateValue . map digitToInt . filter isDigit)


main :: IO ()
main = do
    let filepath :: String = "../input.txt"
    input :: String <- readFile filepath
    let parsed :: [String] = lines input 
    let output :: Int = solve parsed
    print output
