type Report = [Int]

data Slope = Ascending | Descending

parse :: String -> [Report]
parse = map parseLine . lines

parseLine :: String -> Report
parseLine = map read . words

solve :: [Report] -> Int
solve = length . filter checkReport

checkReport :: Report -> Bool
checkReport report = validate report Nothing

validate :: Report -> Maybe Slope -> Bool
validate [] _ = True
validate [_] _ = True
validate (x:y:xs) Nothing
    | x > y && x-3 <= y = validate (y:xs) $ Just Descending
    | x < y && x+3 >= y = validate (y:xs) $ Just Ascending
    | otherwise = False
validate (x:y:_) _ | x == y = False
validate (x:y:_) (Just Descending) | x < y || x-3 > y = False
validate (x:y:_) (Just Ascending) | x > y || x+3 < y = False
validate (_:xs) (Just slope) = validate xs $ Just slope

main :: IO ()
main = do
    let filepath :: String = "../../inputs/input-02.txt"
    input :: String <- readFile filepath
    print $ solve $ parse input
