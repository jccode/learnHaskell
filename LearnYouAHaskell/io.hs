import Data.Char
import Control.Monad

main = do
  -- test1
  -- test2
  -- test3
  -- test4
  -- test5
  -- test6
  -- test7
  -- test8
  -- test9
  -- test10
  -- test11
  -- test12
  -- test12'
  test12''

test1 :: IO ()
test1 = do
  putStrLn "what's your first name?"
  firstName <- getLine
  putStrLn "what's your last name?"
  lastName <- getLine

  let bigFirstName = map toUpper firstName
      bigLastName = map toUpper lastName
  putStrLn $ "hey " ++ bigFirstName ++ " " ++ bigLastName ++ ", how are you?"

test2 :: IO ()
test2 = do
  line <- getLine
  if null line
    then return ()
    else do
      putStrLn $ reverseWords line
      main

reverseWords :: String -> String
reverseWords = unwords . map reverse . words

test3 :: IO ()
test3 = do
  return ()
  return "HAHAHA"
  line <- getLine
  return "BLAH BLAH BLAH"
  return 4
  putStrLn line

test4 :: IO ()
test4 = do
  let line = getLine
  l <- line
  putStrLn $ "you input is: " ++ l

test5 :: IO ()
test5 = do
  a <- return "hello"
  b <- return "yeah!"
  putStrLn $ a ++ " " ++ b

test6 :: IO ()
test6 = do
  let a = "hello"
      b = "yeah!"
  putStrLn $ a ++ " " ++ b

test7 :: IO ()
test7 = do
  c <- getChar
  if c /= ' '
    then do
    putChar c
    main
    else return ()

test8 :: IO ()
test8 = do
  c <- getChar
  when (c /= ' ') $ do
    putChar c
    main
    
test9 :: IO ()
test9 = do
  forever $ do
    putStr "Give me some input: "
    l <- getLine
    putStrLn $ map toUpper l

test10 :: IO ()
test10 = do
  colors <- forM [1..4] (\a -> do
                            putStrLn $ "Which color do you associate with the number " ++ show a ++ "?"
                            color <- getLine
                            return color)
  putStrLn "The colors that you associate with 1, 2, 3 and 4 are: "
  mapM_ putStrLn colors

test11 :: IO ()
test11 = do
  contents <- getContents
  putStr (map toUpper contents)

test12 :: IO ()
test12 = do
  contents <- getContents
  putStr (shortLinesOnly contents)

shortLinesOnly :: String -> String
shortLinesOnly input =
  let allLines = lines input
      shortLines = filter (\line -> length line < 10 && (not $ null line)) allLines
      result = unlines shortLines
  in result

test12' :: IO ()
test12' = do
  interact shortLinesOnly
  
test12'' :: IO ()
test12'' = interact $ unlines . filter (\l -> length l < 10 && (not $ null l)) . lines

