import Data.List
import qualified Data.Map as Map
import qualified Geometry
import qualified Geometry.Sphere
import Shapes


inits' :: [a] -> [[a]]
inits' = scanl (\acc x -> acc ++ [x]) []

--inits' [] = []
--inits' xs = _init : inits' _init
--	where _init = init xs


numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub 


------------------------------------

phoneBook = [("betty","555-2938") ,
			("bonnie","452-2928") , 
			("patsy","493-2928") , 
			("lucille","205-2928") , 
			("wendy","939-8282") , 
			("penny","853-2492")]


findKey :: (Eq k) => k -> [(k, v)] -> Maybe v

--findKey x [] = Nothing
--findKey x ((k, v):xs)
--	| x == k 	= Just v
--	| otherwise = findKey x xs

--findKey x [] = Nothing
--findKey x ((k, v):xs) = if x == k then Just v else findKey x xs

findKey key = foldr (\(k, v) acc -> if k == key then Just v else acc) Nothing


fromList' :: (Ord k) => [(k, v)] -> Map.Map k v
fromList' = foldr (\(k, v) acc -> Map.insert k v acc) Map.empty 




------------------------------------
------------------------------------

data Person = Person { firstName :: String 
					 , lastName :: String
					 , age :: Int
					 , height :: Float
					 , phoneNumber :: String
					 , flavor :: String
					 } deriving (Show, Eq, Read)

data Car = Car { company :: String
			   , model :: String
			   , year :: Int
			   } deriving (Show)

data Maybe' a = Nothing' | Just' a deriving (Show)

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
		   deriving (Eq, Ord, Show, Read, Bounded, Enum)
