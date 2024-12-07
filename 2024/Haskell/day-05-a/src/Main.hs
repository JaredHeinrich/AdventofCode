import Data.List (elemIndex)
type Rule = (Int,Int)
type Update = [Int]


splitOn :: Char -> String -> [String]
splitOn _ "" = []
splitOn c s = s' : splitOn c (drop 1 s'')
    where (s',s'') = break (==c) s

updateFromString :: String -> Update
updateFromString = map read . splitOn ','

ruleFromString :: String -> Rule
ruleFromString = toRule . map read . splitOn '|' where
    toRule :: [Int] -> Rule
    toRule [x,y] = (x,y)
    toRule _ = error "Wrong number of values for rule"

checkRule :: Update -> Rule -> Bool
checkRule update (x,y) = 
    let indexX = elemIndex x update
        indexY = elemIndex y update
    in case (indexX,indexY) of
        (Just a, Just b) -> a < b 
        _ -> True

isUpdateValid :: Update -> [Rule] -> Bool
isUpdateValid update = all $ checkRule update

parse :: String -> ([Rule], [Update])
parse input = do
    let (ruleStrings, updateStrings) = break (=="") (lines input)
    let rules = map ruleFromString ruleStrings
    let updates = map updateFromString (drop 1 updateStrings)
    (rules, updates)

middleOf :: [a] -> a
middleOf arr | even (length arr) = error "Update should have uneven length"
middleOf arr = arr !! div (length arr) 2

solve :: [Rule] -> [Update] -> Int
solve rules = sum . map processUpdate where
    processUpdate :: Update -> Int
    processUpdate update | isUpdateValid update rules = middleOf update
    processUpdate _ = 0

main :: IO()
main = do
    let filepath = "../../testinputs/testinput-05.txt"
    input <- readFile filepath
    let (rules,updates) = parse input
    print $ solve rules updates
