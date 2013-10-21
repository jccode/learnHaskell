

solveRPN :: String -> Float
solveRPN = head . foldl foldingFun [] . words
  where foldingFun (x:y:ys) "*" = (x * y) : ys
        foldingFun (x:y:ys) "+" = (x + y) : ys
        foldingFun (x:y:ys) "-" = (y - x) : ys
        foldingFun (x:y:ys) "/" = (y / x) : ys
        foldingFun (x:xs) "ln"  = log x : xs
        foldingFun xs "sum" = [sum xs]
        foldingFun xs numberString = read numberString : xs


