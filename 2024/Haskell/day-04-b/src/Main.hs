type Grid = [[Char]]
type Vector = (Int,Int)

addVectors :: Vector -> Vector -> Vector
addVectors (a,b) (c,d) = (a+c,b+d)

getCharFromGrid :: Grid -> Vector -> Maybe Char
getCharFromGrid grid (x,y) 
    | x < 0 = Nothing
    | y < 0 = Nothing
    | y >= length grid  = Nothing
    | x >= length (grid !! y)  = Nothing
    | otherwise = Just (grid !! y !! x)

checkCorners :: (Vector -> Maybe Char) -> Vector -> Bool
checkCorners getCharWithPos pos
    | collectCorners == Just "MMSS" = True
    | collectCorners == Just "MSSM" = True
    | collectCorners == Just "SSMM" = True
    | collectCorners == Just "SMMS" = True
    | otherwise = False
    where
    collectCorners :: Maybe [Char]
    collectCorners = mapM (getCharWithPos . addVectors pos) corners
    corners :: [Vector]
    corners = [(1,-1),(1,1),(-1,1),(-1,-1)]

indexGrid :: Grid -> [(Vector, Char)]
indexGrid grid = 
    concatMap (\(i, row) -> zipWith ( \ j el -> ((j, i), el)) [0..] row) (zip [0..] grid)


solve :: (Vector -> Maybe Char) -> [(Vector,Char)] -> Int
solve getCharWithPos = length . filter checkA . filter isA where
    isA :: (Vector, Char) -> Bool
    isA (_,c) = c == 'A'
    checkA :: (Vector, Char) -> Bool
    checkA (pos,_) = checkCorners getCharWithPos pos


parse :: String -> Grid
parse = lines

main :: IO()
main = do
    let filepath = "../../inputs/input-04.txt"
    input <- readFile filepath
    let grid = parse input
    print $ solve (getCharFromGrid grid) (indexGrid grid)
