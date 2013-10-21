

main = do
  -- test1
  test1'


test1 = do
  line <- getLine
  let line' = reverse line
  putStrLn $ "You said " ++ line' ++ " backwards!"
  putStrLn $ "Yes, you really said" ++ line' ++ " backwards!"

test1' = do
  line <- fmap reverse getLine
  putStrLn $ "You said " ++ line ++ " backwards!"
  putStrLn $ "Yes, you really said" ++ line ++ " backwards!"
  
