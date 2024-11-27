import Data.Maybe (fromJust)
parse :: String -> [(Move, Move)]
parse input = map parseLine (lines input)

parseLine :: String -> (Move, Move)
parseLine line = (parseChar $ head line, parseChar $ last line) where
    parseChar = fromJust . charToMove


data Result = Win | Lose | Draw
data Move = Rock | Paper | Scissors
    deriving(Show, Eq)

charToMove :: Char -> Maybe Move
charToMove 'A' = Just Rock
charToMove 'B' = Just Paper
charToMove 'C' = Just Scissors

charToMove 'X' = Just Rock
charToMove 'Y' = Just Paper
charToMove 'Z' = Just Scissors
charToMove _ = Nothing

determineGameResult :: (Move, Move) -> Result
determineGameResult (a,b) | a == b = Draw
determineGameResult (Rock, Paper) = Win
determineGameResult (Paper, Scissors) = Win
determineGameResult (Scissors, Rock) = Win
determineGameResult _ = Lose

calculateGamePoints :: (Move, Move) -> Int
calculateGamePoints (hisMove, myMove) = points myMove + points (determineGameResult (hisMove, myMove))

solve :: [(Move, Move)] -> Int
solve = sum . map calculateGamePoints

class PointSystem a where
    points :: a -> Int

instance PointSystem Move where
    points Rock = 1
    points Paper = 2
    points Scissors = 3

instance PointSystem Result where
    points Lose = 0
    points Draw = 3
    points Win = 6


main :: IO ()
main = do
    let filepath :: String = "../../inputs/input-02.txt"
    input :: String <- readFile filepath
    print $ solve $ parse input
