
import System.Directory

main = do
  putStrLn "main"
  let files = [x | x <- getDirectoryContents ]
  mapM_ putStrLn files
