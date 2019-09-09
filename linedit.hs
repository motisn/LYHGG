import System.IO
import Data.Char
import Control.Monad.State

type StrZipper = (String,String)

{-- Stateモナド使ったらきれいになる？
goForward :: State StrZipper ()
goForward = state $ \(x:xs, bs) -> ((), (xs, x:bs))
goBack :: State StrZipper ()
goBack = state $ \(xs, b:bs) -> ((), (b:xs, bs))
insertCh :: Char -> State StrZipper ()
insertCh x = state $ ((), (x:xs, bs))
--}
goForward :: StrZipper -> StrZipper
goForward (x:xs, bs) = (xs, x:bs)
goBack :: StrZipper -> StrZipper
goBack (xs, b:bs) = (b:xs, bs)
insertCh :: Char -> StrZipper -> StrZipper
insertCh b (xs, bs) = (xs, b:bs)
backSpace :: StrZipper -> StrZipper
backSpace (xs, b:bs) = (xs, bs)

getKey :: IO Int
getKey = do
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    c <- getChar
    hSetEcho stdin True
    return $ ord c

editor (xs, bs) = do
    print $ reverse bs ++ "|" ++ xs
    input <- getKey
    case input of
        27 -> return () -- esc
        127 -> do -- bask space
            editor $ backSpace (xs, bs)
        62 -> do -- '>'
            editor $ goForward (xs, bs)
        60 -> do -- '<'
            editor $ goBack (xs, bs)
        input -> do
            --putStrLn $ show input ++ " was input."
            if (32 <= input) && (input <= 126)
                then
                    let (xs', bs') = insertCh (chr input) (xs, bs)
                    in  editor (xs', bs')
                else
                    let (xs', bs') = (xs, bs)
                    in  editor (xs', bs')

main = do
    editor ([], [])