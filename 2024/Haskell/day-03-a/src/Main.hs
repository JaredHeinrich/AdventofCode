import Data.Char (isDigit)
import Debug.Trace (trace)
data State = 
    Start |
    Find |
    M |
    U |
    L |
    OpeningBracket |
    NumberOne |
    Komma |
    NumberTwo |
    ClosingBracket 
    deriving (Eq)

transition :: State -> Char -> State
transition _ 'm' = M
transition M 'u' = U
transition U 'l' = L
transition L '(' = OpeningBracket
transition OpeningBracket c | isDigit c = NumberOne
transition NumberOne c | isDigit c = NumberOne
transition NumberOne ',' = Komma
transition Komma c | isDigit c = NumberTwo
transition NumberTwo c | isDigit c = NumberTwo
transition NumberTwo ')' = ClosingBracket
transition _ _ = Find

runDEA :: String -> Int -> (String, String) -> State -> Int
runDEA [] count _ _ = count
runDEA [_] count _ _ = count
runDEA (c:cs) count buffer Start = runDEA (c:cs) count buffer $ transition Start c
runDEA (p:c:cs) count (one,two) state
    | state == NumberOne = runDEA (c:cs) count (one ++ [p],two) $ transition state c
    | state == NumberTwo = runDEA (c:cs) count (one,two ++ [p]) $ transition state c
    | state == ClosingBracket = trace ("one:" ++ one ++ "two:" ++ two) runDEA (c:cs) (count + read one * read two) ("","") $ transition state c
    | state == M = runDEA (c:cs) count ("","") $ transition state c
    | otherwise = runDEA (c:cs) count (one,two) $ transition state c

solve :: String -> Int
solve input = runDEA input 0 ("","") Start

main :: IO()
main = do
    let filepath = "../../inputs/input-03.txt"
    input <- readFile filepath
    print $ solve input
