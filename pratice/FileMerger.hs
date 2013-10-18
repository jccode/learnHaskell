
import System.Directory
import Data.List


main = do
  putStrLn "Hello FileMerger beginning..."
  let dir = "d:/HGST/MFG/processing/HDD_WEBSPC_CR/C137/log/"
      destFile = "./a.txt"
  files <- getDirectoryContents dir
  let filesToBeProcess = [x | x <- files, isInfixOf ".log" x]
  mapM_ putStrLn filesToBeProcess
  -- mergeToOneFile destFile filesToBeProcess
  mergeToOneFile destFile files


mergeToOneFile :: FilePath -> [FilePath] -> IO ()
mergeToOneFile file inputFilePath = do
  putStrLn "merge to file"


