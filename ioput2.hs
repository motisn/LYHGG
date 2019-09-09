import Data.Char
import System.Random
--import System.IO

--リロードしたら動かない
main :: IO ()
main = interact capsLocker
capsLocker :: String -> String
capsLocker = unlines . map (map toUpper) . lines
--main = do 
--    contents <- getContents --getContents = hGetContents stdin
--    putStr $ map toUpper contents

randomPass :: (RandomGen g) => g -> String
randomPass gen = map chr $ randomRs (33,126) gen