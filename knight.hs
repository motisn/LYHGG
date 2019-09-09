import Control.Monad

type KnightPos = (Int, Int) -- Column Row
moveKnight :: KnightPos -> [KnightPos]
moveKnight (c, r) = do
    (c', r') <- [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1),(c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)]
    guard (c' `elem` [1..8] && r' `elem` [1..8])
    return (c',r')

in3 start = return start >>= moveKnight >>= moveKnight >>= moveKnight

canReachIn3 :: KnightPos -> KnightPos -> Bool
canReachIn3 start end = end `elem` in3 start

type KnightPath = [KnightPos]
traceKnight :: KnightPath -> [KnightPath]
traceKnight path = do
    end <- moveKnight $ head path
    return $ end:path

showPathIn3 :: KnightPos -> KnightPos -> [KnightPath]
{--
showPathIn3 start end = do
    first <- moveKnight start
    second <- moveKnight first
    third <- moveKnight second
    guard (third == end)
    return $ [start] ++ [first] ++ [second] ++ [third]
--}
showPathIn3 start end = traceKnight [start] >>= traceKnight >>= traceKnight >>= (\all@(x:xs) -> guard (x == end) >> return all)

inN :: Int -> KnightPos -> [KnightPos]
inN n start = return start >>= foldr (<=<) return (replicate n moveKnight)

canReachInN :: Int -> KnightPos -> KnightPos -> Bool
canReachInN n start end = end `elem` inN n start

showPathInN :: Int -> KnightPos -> KnightPos -> [KnightPath]
showPathInN n start end = return [start] >>= foldr (<=<) return (replicate n traceKnight) >>= (\all@(x:xs) -> guard (x == end) >> return all)