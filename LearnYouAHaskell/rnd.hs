import System.Random
import Data.List

main = do
  -- test1
  
  -- test2
  -- test3
  -- test4

  -- test5
  test6
  
test1 = do
  gen <- getStdGen
  putStrLn $ take 20 (randomRs ('a', 'z') gen)
  gen2 <- getStdGen
  putStrLn $ take 20 (randomRs ('a', 'z') gen2)

test2 = do
  gen <- getStdGen
  let rndChars = randomRs ('a','z') gen
      (first20, rest) = splitAt 20 rndChars
      (second20, _) = splitAt 20 rest
  putStrLn first20
  putStrLn second20

test3 = do
  gen <- getStdGen
  let rndChars = randomRs ('a', 'z') gen
      charsGroup = get20Chars rndChars
  mapM_ putStrLn $ map (charsGroup !!) [0,1]

test4 = do
  gen <- getStdGen
  let rndChars = randomRs ('a', 'z') gen
      charsGroup = nBitPerGroup 20 rndChars
  mapM_ putStrLn $ map (charsGroup !!) [0,1]
  
get20Chars :: [a] -> [[a]]
get20Chars chars =
  let (first, rest) = splitAt 20 chars
  in first:get20Chars rest
     
nBitPerGroup :: Int -> [a] -> [[a]]
nBitPerGroup n chars =
  let (first, rest) = splitAt n chars
  in first:nBitPerGroup n rest
     

test5 = do
  gen <- getStdGen
  putStrLn $ take 20 (randomRs ('a', 'z') gen)
  gen' <- newStdGen
  putStrLn $ take 20 (randomRs ('a', 'z') gen')

