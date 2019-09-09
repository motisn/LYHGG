import Data.Char
import Control.Monad

main = do
    line <- getLine
--    when (not $ null line) $ do 
--        putStrLn $ reverseWords line
--        main
-- 終わらないやつ
    if null line
        then do
--            putStrLn "Opps!"
            main
-- これはwhenと同じ
--    if null line
--        then return ()
        else do
            putStrLn' $ reverseWords line
            main

reverseWords :: String -> String
reverseWords = unwords . map reverse . words

putStrLn' :: String -> IO ()
putStrLn' x = putStr' $ x ++ "\n"

putStr' :: String -> IO ()
putStr' [] = return ()
putStr' (x:xs) = do
    putChar x
    putStr' xs