import System.Environment
import System.Directory
import System.IO
import Control.Exception
import Data.List

dispatch :: String -> [String] -> IO ()
dispatch "add" = add
dispatch "view" = view
dispatch "remove" = remove
--dispatch "swap" = swap
dispatch command = usage command

main = do
    args <- getArgs
    let (command:argList) = if args == [] then ["[]"] else args
    dispatch command argList

usage :: String -> [String] -> IO ()
usage command _ = do
    progName <- getProgName
    putStrLn $ "An error occured in " ++ progName ++ ":"
    putStrLn $ "    " ++ command ++ " is invalid input."

add :: [String] -> IO ()
add [fileName, todoItem] = do
    contents <- readFile fileName
    if last contents == '\n'
        then appendFile fileName (todoItem)
        else appendFile fileName ("\n" ++ todoItem)

view :: [String] -> IO ()
view [fileName] = (do
    contents <- readFile fileName
    let todoTasks = lines contents
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks
    putStr $ unlines numberedTasks) `catch` puterror -- $ (SomeException e) => (\e putStrLn "Error")
view otherwise = usage "[]" otherwise

puterror :: SomeException -> IO ()
puterror _ = putStrLn "Error"

remove :: [String] -> IO ()
remove [fileName, numberString] = do
    contents <- readFile fileName
    let todoTasks = lines contents
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks
    putStrLn "There are your TO-DO items:"
    mapM_ putStrLn numberedTasks
    -- view filename
    let number = read numberString
        newTodoItems = unlines $ delete (todoTasks !! number) todoTasks
    bracketOnError (openTempFile "." "temp")
        (\(tempName, tempHandle) -> do
            hClose tempHandle
            removeFile tempName)
        (\(tempName, tempHandle) -> do
            hPutStr tempHandle newTodoItems
            hClose tempHandle
            removeFile fileName
            renameFile tempName fileName)