
import System.Environment (getArgs)

interactWith fn inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile (fn input)

main = mainWith myFun
  where mainWith fun = do
          args <- getArgs
          case args of [input, output] -> interactWith fun input output
                       _ -> putStrLn "error: exactly two arguments needed"

        -- replace "id" with the name of our function below
        myFun = id

