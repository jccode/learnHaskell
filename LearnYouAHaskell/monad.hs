import Control.Monad.Writer

lookFold :: Int -> Int -> Writer [String] Int
lookFold acc x = do
  tell ["currently x = " ++ (show x) ++ ", and acc = " ++ (show acc)]
  return (acc + x)
  
