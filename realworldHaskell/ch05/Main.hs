
module Main (main) where

import SimpleJSON
import PutJSON

main = do
  let data1 = JObject [("foo", JNumber 1), ("bar", JBool False)]
  print data1
  putJValue data1
