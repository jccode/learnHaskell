import Data.List
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

