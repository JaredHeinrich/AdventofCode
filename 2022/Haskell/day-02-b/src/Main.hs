import Data.Maybe (fromJust)
parse :: String -> [(Move, Result)]
parse input = map parseLine (lines input)

parseLine :: String -> (Move, Result)
parseLine line = (fromJust . fromChar $ head line, fromJust . fromChar $ last line)

data Result = Win | Lose | Draw
data Move = Rock | Paper | Scissors
    deriving(Show, Eq)

class GameSymbol a where
    points :: a -> Int
    fromChar :: Char -> Maybe a

instance GameSymbol Move where
    points Rock = 1
    points Paper = 2
    points Scissors = 3
    fromChar 'A' = Just Rock
    fromChar 'B' = Just Paper
    fromChar 'C' = Just Scissors
    fromChar _ = Nothing

instance GameSymbol Result where
    points Lose = 0
    points Draw = 3
    points Win = 6
    fromChar :: Char -> Maybe Result
    fromChar 'X' = Just Lose
    fromChar 'Y' = Just Draw
    fromChar 'Z' = Just Win
    fromChar _ = Nothing

determineMyMove :: (Move, Result) -> Move
determineMyMove (hismove, Draw) = hismove
determineMyMove (hismove, Win)
    | hismove == Rock = Paper
    | hismove == Paper = Scissors
    | otherwise = Rock

determineMyMove (hismove, Lose)
    | hismove == Rock = Scissors
    | hismove == Paper = Rock
    | otherwise = Paper

calculateGamePoints :: (Move, Result) -> Int
calculateGamePoints (hisMove, result) = points result + points (determineMyMove (hisMove, result))

solve :: [(Move, Result)] -> Int
solve = sum . map calculateGamePoints



main :: IO ()
main = do
    let filepath :: String = "../../inputs/input-02.txt"
    input :: String <- readFile filepath
    print $ solve $ parse input
