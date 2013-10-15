import System.IO
import Data.Char
import Data.List
import System.Directory
import System.Environment

main = do
  -- test1
  -- test2
  -- test3
  -- test4
  -- addToDoItem
  -- deleteToDoItem
  cmdargs


test1 :: IO ()  
test1 = do  
  handler <- openFile "hello.hs" ReadMode
  contents <- hGetContents handler
  putStr contents
  hClose handler
  

test2 :: IO ()
test2 = do
  withFile "hello.hs" ReadMode (\handle -> do
                                   contents <- hGetContents handle
                                   putStr contents)

test3 :: IO ()
test3 = do
  contents <- readFile "hello.hs"
  putStr contents
  

test4 :: IO ()
test4 = do
  contents <- readFile "hello.hs"
  writeFile "a.txt" (map toUpper contents)


addToDoItem :: IO ()
addToDoItem = do
  todoItem <- getLine
  appendFile "todo.txt" $ todoItem ++ "\n"

deleteToDoItem :: IO ()
deleteToDoItem = do
  contents <- readFile "todo.txt"
  (tempName, tempHandle) <- openTempFile "." "temp"
  let taskItems = lines contents
      tasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] taskItems
  putStrLn "These are your TO-DO items:"
  mapM putStrLn tasks
  putStrLn "Which one do you want to delete?"
  numStr <- getLine
  let num = read numStr
      newTodoItems = delete (taskItems !! num) taskItems
  mapM (hPutStrLn tempHandle) newTodoItems
  hClose tempHandle
  removeFile "todo.txt"
  renameFile tempName "todo.txt"


cmdargs :: IO ()
cmdargs = do
  args <- getArgs
  progName <- getProgName
  putStrLn "The arguments are: "
  mapM putStrLn args
  putStrLn "The program name is: "
  putStrLn progName
  
