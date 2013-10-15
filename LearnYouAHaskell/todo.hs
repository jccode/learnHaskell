import System.Environment
import System.IO
import System.Directory
import Data.List

dispatch :: [(String, [String] -> IO ())]
dispatch = [("add", add),
            ("view", view),
            ("remove", remove)]

main = do
  (cmd:args) <- getArgs
  let (Just action) = lookup cmd dispatch
  action args

add :: [String] -> IO ()
add [fileName, todoItem] = do
  appendFile fileName (todoItem ++ "\n")


view :: [String] -> IO ()
view [fileName] = do
  contents <- readFile fileName
  let todoTasks = lines contents
      numTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks
  putStr $ unlines numTasks


remove :: [String] -> IO ()
remove [fileName, numberString] = do
  contents <- readFile fileName
  (tempName, tempHandle) <- openTempFile "." "temp"
  let taskItems = lines contents
      num = read numberString
      newTodoItems = delete (taskItems !! num) taskItems
  mapM (hPutStrLn tempHandle) newTodoItems
  hClose tempHandle
  removeFile fileName
  renameFile tempName "todo.txt"

