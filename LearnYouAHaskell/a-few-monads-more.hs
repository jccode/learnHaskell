import Data.Monoid
import Control.Monad.Trans.State


isBigGang :: Int -> (Bool, String)
isBigGang x = (x > 9, "Compared gang size to 9")


-- applyLog :: (a, String) -> (a -> (b, String)) -> (b, String)
-- applyLog (x, log) f = let (y, newLog) = f x in (y, log ++ newLog)


applyLog :: (Monoid m) => (a, m) -> (a -> (b, m)) -> (b, m)
applyLog (x, log) f = let (y, newLog) = f x in (y, log `mappend` newLog)


type Food = String
type Price = Sum Int

addDrink :: Food -> (Food, Price)
addDrink "beans" = ("milk", Sum 25)
addDrink "jerky" = ("whiskey", Sum 99)
addDrink _ = ("bear", Sum 30)


type Stack = [Int]

pop :: Stack -> (Int, Stack)
pop (x:xs) = (x, xs)

push :: Int -> Stack -> ((), Stack)
push a xs = ((), a:xs)


stackManip :: Stack -> (Int, Stack)
stackManip stack = let
    ((), newStack1) = push 3 stack
    (a, newStack2) = pop newStack1
    in pop newStack2

-- stackManip [1..5]


pop2 :: State Stack Int
pop2 = state $ \(x:xs) -> (x,xs)

push2 :: Int -> State Stack ()
push2 x = state $ \xs -> ((), x:xs)

stackManip2 :: State Stack Int
stackManip2 = do
    push2 3
    a <- pop2
    pop2

-- runState stackManip2 [1..5]
