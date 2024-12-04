import Data.Char (isDigit)
data State = 
    Start |
    Do |
    Dont |
    M |
    U |
    L |
    MulOpenBracket |
    FirstNumber |
    Komma |
    SecondNumber |
    MulClosingBracket |
    DoD |
    DoO |
    DoOpenBracket |
    DontD |
    DontO |
    DontN |
    DontQuote |
    DontT |
    DontOpenBracket
    deriving (Eq)

transition :: State -> Char -> State
transition Do c
    | c == 'd' =  DontD
    | c == 'm' = M
    | otherwise = Do
transition DontD c 
    | c == 'o' = DontO
    | c == 'd' = DontD
    | c == 'm' = M
    | otherwise = Do
transition DontO c
    | c == 'n' = DontN
    | c == 'd' = DontD
    | c == 'm' = M
    | otherwise = Do
transition DontN c
    | c == '\'' = DontQuote
    | c == 'd' = DontD
    | c == 'm' = M
    | otherwise = Do
transition DontQuote c
    | c == 't' = DontT
    | c == 'd' = DontD
    | c == 'm' = M
    | otherwise = Do
transition DontT c
    | c == '(' = DontOpenBracket
    | c == 'd' = DontD
    | c == 'm' = M
    | otherwise = Do
transition DontOpenBracket c
    | c == ')' = Dont
    | c == 'd' = DontD
    | c == 'm' = M
    | otherwise = Do

transition Dont c
    | c == 'd' = DoD
    | otherwise = Dont
transition DoD c
    | c == 'o' = DoO
    | c == 'd' = DoD
    | otherwise = Dont
transition DoO c
    | c == '(' = DoOpenBracket
    | c == 'd' = DoD
    | otherwise = Dont
transition DoOpenBracket c
    | c == ')' = Do
    | c == 'd' = DoD
    | otherwise = Dont

transition M c
    | c == 'u' = U
    | c == 'd' = DontD
    | c == 'm' = M
    | otherwise = Do
transition U c
    | c == 'l' = L
    | c == 'd' = DontD
    | c == 'm' = M
    | otherwise = Do
transition L c
    | c == '(' = MulOpenBracket
    | c == 'd' = DontD
    | c == 'm' = M
    | otherwise = Do
transition MulOpenBracket c
    | isDigit c = FirstNumber
    | c == 'd' = DontD
    | c == 'm' = M
    | otherwise = Do
transition FirstNumber c
    | isDigit c = FirstNumber
    | c == ',' = Komma
    | c == 'd' = DontD
    | c == 'm' = M
    | otherwise = Do
transition Komma c
    | isDigit c = SecondNumber
    | c == 'd' = DontD
    | c == 'm' = M
    | otherwise = Do
transition SecondNumber c
    | isDigit c = SecondNumber
    | c == ')' = MulClosingBracket
    | c == 'd' = DontD
    | c == 'm' = M
    | otherwise = Do
transition MulClosingBracket c
    | c == 'd' = DontD
    | c == 'm' = M
    | otherwise = Do
transition Start c
    | c == 'd' = DontD
    | c == 'm' = M
    | otherwise = Do


runDEA :: String -> Int -> (String, String) -> State -> Int
runDEA [] count _ _  = count
runDEA [_] count _ _  = count
runDEA (c:cs) count buffer Start = runDEA (c:cs) count buffer $ transition Start c
runDEA (p:c:cs) count (one,two) state
    | state == FirstNumber = runDEA (c:cs) count (one ++ [p],two) $ transition state c
    | state == SecondNumber = runDEA (c:cs) count (one,two ++ [p]) $ transition state c
    | state == MulClosingBracket = runDEA (c:cs) (count + read one * read two) ("","") $ transition state c
    | state == M = runDEA (c:cs) count ("","") $ transition state c
    | otherwise = runDEA (c:cs) count (one,two) $ transition state c

solve :: String -> Int
solve input = runDEA input 0 ("","") Start

main :: IO()
main = do
    let filepath = "../../inputs/input-03.txt"
    input <- readFile filepath
    print $ solve input
