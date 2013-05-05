doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe y
doubleSmallNumber x = if x > 100 then x else x*2
doubleSmallNumber' x = (if x > 100 then x else x*2) + 1
boomBangs xs = [if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

removeNonUppercase :: String -> String
removeNonUppercase st = [c | c <- st, c `elem` ['A'..'Z']]

-- addThree :: Int -> Int -> Int -> Int
addThree :: Num a => a -> a -> a -> a
--addThree x y z = x + y + z
--addThree = \x y z -> x + y + z
--addThree = \x y -> (\z -> x + y + z)
--addThree = \x -> (\y -> (\z -> x + y + z))
addThree = \x -> \y -> \z -> x + y + z


lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of lucky, pal!"

sayMe :: (Integral a) => a -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe 4 = "Four!"
sayMe 5 = "Five!"
sayMe n = "Not between 1 and 5"

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

head' :: [a] -> a
head' [] = error "can't call head on an empty list, dummy!"
head' (x:_) = x

--head' = foldl1 (\x _ -> x)


length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

capital :: String -> String
capital "" = "Empty string, whoops!"
capital all@(x:_) = "The first letter of " ++ all ++ " is " ++ [x]

max' :: (Ord a) => a -> a -> a
max' a b
	| a > b 	= a
	| otherwise	= b


bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
	| bmi <= skinny 	= "You're underweight, you emo, you!"
	| bmi <= 25.0 	= "You're supposedly normal. Pffft, I bet you're ugly!"
	| bmi <= 30.0 		= "You're fat! Lose some weight, fatty!"
	| otherwise 		= "You're a whale, congratulations!"
    where bmi = weight / height ^ 2  
          (skinny, normal, fat) = (18.5, 25.0, 30.0)


--initials :: String -> String -> String
--initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
--    where (f:_) = firstname
--          (l:_) = lastname

initials :: String -> String -> String
initials (f:_) (l:_) = [f] ++ ". " ++ [l] ++ "."


calcBmis :: (RealFloat a) => [(a, a)] -> [a]
--calcBmis xs = [ w / h ^ 2 | (w, h) <- xs ]
--calcBmis xs = [ bmi w h | (w, h) <- xs ]
--    where bmi weight height = weight / height ^ 2
calcBmis xs = [ bmi| (w, h) <- xs, let bmi = w / h ^ 2, bmi >= 25.0 ]


cylinder :: (RealFloat a) => a -> a -> a
cylinder r h = 
	let sideArea = 2 * pi * r * h
	    topArea = pi * r ^ 2
	in sideArea + 2 * topArea

--cylinder r h = sideArea + 2 * topArea
--    where sideArea = 2 * pi * r * h
--          topArea = pi * r ^ 2


describeList :: [a] -> String
describeList xs = "The List is " ++ what xs
    where what [] = "empty."
          what [x] = "a singleton list."
          what xs = "a longer list."


maximum' :: (Ord a) => [a] -> a
--maximum' [] = error "maximum of empty list"
--maximum' [x] = x
--maximum' (x:xs) 
--	| x > maxTail = x
--	| otherwise = maxTail
--	 where maxTail = maximum' xs

--maximum' [] = error "maximum of empty list"
--maximum' [x] = x
--maximum' (x:xs) = max x (maximum' xs)

--maximum' xs = foldl (\acc x -> if x > acc then x else acc) (xs!!0) xs
maximum' = foldr1 (\x acc -> if x > acc then x else acc)


replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x 
	| n <= 0 		= []
	| otherwise 	= x:replicate' (n-1) x


take' :: (Num a, Ord a) => a -> [b] -> [b]
take' n _ 
	| n <= 0 	= []
take' _ [] 	= []
take' n (x:xs) = x: take' (n-1) xs


reverse' :: [a] -> [a]
--reverse' [] = []
--reverse' (x:xs) = reverse' xs ++ [x]

--reverse' = foldl (\acc x -> x:acc) []
reverse' = foldl (flip (:)) []



repeat' :: a -> [a]
repeat' x = x: repeat' x


zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x, y): zip' xs ys


elem' :: (Eq a) => a -> [a] -> Bool
--elem' a [] = False
--elem' a (x:xs) = a == x || elem' a xs

--elem' y ys = foldl (\acc x -> acc || if x == y then True else False) False ys
elem' y ys = foldl (\acc x -> if x == y then True else acc) False ys



quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
--quicksort (x:xs) = [m | m <- quicksort(xs), m <= x ] ++ [x] ++ [m | m <- quicksort(xs), m > x]

--quicksort (x:xs) = smaller ++ [x] ++ bigger 
--	where smaller = quicksort [m | m <- xs, m <= x]
--	      bigger = quicksort [m | m <- xs, m > x]

--quicksort (x:xs) = 
--	let smaller = quicksort [m | m <- xs, m <= x]
--	    bigger = quicksort [m | m <- xs, m > x]
--	in smaller ++ [x] ++ bigger

quicksort (x:xs) = 
	let smaller = quicksort (filter (<x) xs)
	    bigger = quicksort (filter (>x) xs)
	in smaller ++ [x] ++ bigger


multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z


compareWithHundred :: (Num a, Ord a) => a -> Ordering
--compareWithHundred x = compare 100 x
compareWithHundred = compare 100 


divideByTen :: (Floating a) => a -> a
divideByTen = (/10)
--divideByTen a = a/10


isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'])

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)


zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys



flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f x y = f y x


chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain x 
	| even x 	= x : chain (x `div` 2)
	| otherwise = x : chain (x * 3 + 1)


numLongChains :: Int
--numLongChains = length (filter isLong (map chain [1..100]))
--	where isLong xs = length xs > 15
numLongChains = length (filter (\xs -> length xs > 15) (map chain [1..100]))


sum' :: (Num a) => [a] -> a
--sum' [] = 0
--sum' (x:xs) = x + sum' xs

--sum' xs = foldl (\acc x -> acc + x) 0 xs
sum' = foldl (+) 0

foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' _ a [] = a
foldl' f a (x:xs) = foldl' f (f a x) xs


map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc) [] xs


product' :: (Num a) => [a] -> a
product' [] = 1
product' xs = foldr1 (*) xs


filter' :: (a -> Bool) -> [a] -> [a]
--filter' f xs = foldr (\x acc -> if f x then x:acc else acc) [] xs
filter' f = foldr (\x acc -> if f x then x:acc else acc) []


oddSquareSum :: Integer
--oddSquareSum = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))
--oddSquareSum = sum . takeWhile (<10000) . filter odd . map (^2) $ [1..]
oddSquareSum = 
	let oddSquares = filter odd $ map (^2) [1..]
	    belowLimit = takeWhile (<10000) oddSquares
	in sum belowLimit

