type Grid = [[Char]]
type Vector = (Int,Int)

addVectors :: Vector -> Vector -> Vector
addVectors (a,b) (c,d) = (a+c,b+d)

scaleVector :: Vector -> Int -> Vector
scaleVector (a,b) l = (a*l,b*l)

getCharFromGrid :: Grid -> Vector -> Maybe Char
getCharFromGrid grid (x,y) 
    | x < 0 = Nothing
    | y < 0 = Nothing
    | y >= length grid  = Nothing
    | x >= length (grid !! y)  = Nothing
    | otherwise = Just (grid !! y !! x)

checkDirections :: (Vector -> Maybe Char) -> Vector -> Int
checkDirections getCharWithPos pos = length $ filter checkDirection directions where
    directions :: [Vector]
    directions = [(0,-1),(1,-1),(1,0),(1,1),(0,1),(-1,1),(-1,0),(-1,-1)]

    checkDirection :: Vector -> Bool
    checkDirection dir =
        getCharWithPos (addVectors pos (scaleVector dir 1))  == Just 'M' &&
        getCharWithPos (addVectors pos (scaleVector dir 2)) == Just 'A' &&
        getCharWithPos (addVectors pos (scaleVector dir 3)) == Just 'S' 

indexGrid :: Grid -> [(Vector, Char)]
indexGrid grid = 
    concatMap (\(i, row) -> zipWith ( \ j el -> ((j, i), el)) [0..] row) (zip [0..] grid)


solve :: (Vector -> Maybe Char) -> [(Vector,Char)] -> Int
solve getCharWithPos = sum . map checkX . filter isX where
    isX :: (Vector, Char) -> Bool
    isX (_,c) = c == 'X'
    checkX :: (Vector, Char) -> Int
    checkX (pos,_) = checkDirections getCharWithPos pos


parse :: String -> Grid
parse = lines

main :: IO()
main = do
    let filepath = "../../inputs/input-04.txt"
    input <- readFile filepath
    let grid = parse input
    print $ solve (getCharFromGrid grid) (indexGrid grid)
