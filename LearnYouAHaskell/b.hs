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


type PhoneBook = [(Name, PhoneNumber)]
type Name = String
type PhoneNumber = String
inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
--inPhoneBook name number book = if ret == Nothing then False else  ret == Just number
--	where ret = Map.lookup name $ Map.fromList book
inPhoneBook name pnumber pbook = (name,pnumber) `elem` pbook



data LockerState = Taken | Free deriving (Show, Eq)
type Code = String
type LockerMap = Map.Map Int (LockerState, Code)
lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map = 
	case Map.lookup lockerNumber map of 
		Nothing -> Left $ "Locker number " ++ show lockerNumber ++ " doesn't exist!"
		--Just (state, code) -> if state /= Taken 
		--						then Right code 
		--						else Left $ "Locker " ++ show lockerNumber ++ " is already taken!"
		Just (state, code) -> case state of 
									Taken -> Left $ "Locker " ++ show lockerNumber ++ " is already taken!"
									Free -> Right code 

lockers :: LockerMap
lockers = Map.fromList [
	(100, (Taken, "ZD39I")), 
	(101, (Free, "JAH3I")), 
	(102, (Free, "IQWA0")), 
	(103, (Free, "IEYI9")), 
	(105, (Taken, "9BDEL")), 
	(109, (Taken, "87HFK")), 
	(110, (Taken, "ZD3MN"))
	]


