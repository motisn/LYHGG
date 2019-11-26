solveRPN :: String -> Double
solveRPN = head . foldl foldingFunc [] . words
    --where foldingFunc acc itemFromList = ...
    where foldingFunc (x:y:ys) "*" = (y * x):ys
          foldingFunc (x:y:ys) "+" = (y + x):ys
          foldingFunc (x:y:ys) "-" = (y - x):ys
          foldingFunc xs numberString = read numberString:xs

solveS :: String -> String
solveS x = let result = step x
            in if length (words result) == 1 then result else solveS result

step :: String -> String
step = fst . foldl (\(xs, ys) -> \c ->
    (case c of
        '(' -> (xs ++ ys, "(")
        ')' -> (xs ++ (if ys == [] then ")" else calc (tail ys)), [])
        token -> (xs, ys ++ [token])
    )) ([], [])

calc :: String -> String
calc s = let tokens = words s in 
    case tokens of
        "+":nums -> show . sum . map read $ nums
        "*":nums -> show . product . map read $ nums
        otherwise -> unwords tokens
{--
solveS :: String -> String
solveS x = let result = iter x
           in if length (words result) == 1 then result else iter result

iter :: String -> String
iter = unwords . fst . foldl parse ([], []) . words
    where parse (xs, ys) "(" = (xs ++ ys, ["("])
          parse (xs, ys) ")" = (xs ++ (calc ys), [])
          parse (xs, ys) token = (xs, ys ++ [token])

calc :: [String] -> [String]
calc ("(":"+":xs) = words . show . sum . map read $ xs
calc ("(":"*":xs) = words . show . product . map read $ xs
calc xs = xs ++ [")"]
--}

data Section = Section { getA :: Int, getB :: Int, getC :: Int} deriving (Show)
type RoadSystem = [Section]

heathrowToLondon :: RoadSystem
heathrowToLondon = [ Section 50 10 30
                   , Section 5 90 20
                   , Section 40 2 25
                   , Section 10 8 0
                   ]

data Label = A | B | C deriving (Show)
type Path = [(Label, Int)]

optimalPath :: RoadSystem -> Path
optimalPath roadSystem =
    let (bestAPath, bestBPath) = foldl roadStep ([], []) roadSystem
    in if sum (map snd bestAPath) <= sum (map snd bestBPath)
        then reverse bestAPath
        else reverse bestBPath

roadStep :: (Path, Path) -> Section -> (Path, Path)
roadStep (pathA, pathB) (Section a b c) =
    let timeA = sum (map snd pathA)
        timeB = sum (map snd pathB)
        forwardTimeToA = timeA + a
        crossTimeToA = timeB + b + c
        forwardTimeToB = timeB + b
        crossTimeToB = timeA + a + c
        newPassToA = if forwardTimeToA <= crossTimeToA
            then (A, a):pathA
            else (C, c):(B, b):pathB
        newPassToB = if forwardTimeToB <= crossTimeToB
            then (B, b):pathB
            else (C, c):(A, a):pathA
        in (newPassToA, newPassToB)