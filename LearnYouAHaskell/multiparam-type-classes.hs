{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}


-- Multi-parameter type classes AND Functional dependencies

class Eq e => Collection c e | c -> e where
    insert :: c -> e -> c
    member :: c -> e -> Bool

instance Eq a => Collection [a] a where
    insert xs x = x:xs
    member = flip elem


-- insert [1..5] 3
-- member [1..5] 3