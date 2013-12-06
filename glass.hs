{-# LANGUAGE ScopedTypeVariables, ViewPatterns #-}
import Data.Map hiding (foldr, split,map, filter,null)

import System.IO
import System.Random hiding (split)
import Text.CSV


-- stepping structure
type G a = Map Int a 

-- given and index to an element and a splitting function eliminate the element splitting it in two new ones
split :: Int -> (a -> (a,a)) -> G a -> G a
split i f m = insert i x . insert (lk m + 1) y $ m where
                (x,y) = f (m ! i)
                lk  = fst . findMax 

-- collision function
type T a b = b -> a -> a -> (a,a)

-- collision parameter with target index
data E b = E Int b deriving Show

-- target enumeration
enumTa :: Int -> [([[Int]],Int)]
enumTa n = enumTa where
        enumTa = (map return [0..n-1],n):map g enumTa 
        g :: ([[Int]],Int) ->  ([[Int]],Int)
        g (ys,n) = (concat [map (i:) ys  | i <- [0..n]], n + 1)

 
-- collisions description
type GlassS a b = [(E b, a)]

-- apply collisions description to a state given a collision function
runT :: T a b -> G a -> GlassS a b -> G a 
runT f  = foldr (op f) 

streamT :: T a b -> G a -> GlassS a b -> [G a]
streamT f = scanl $ flip (op f) 

op :: T a b -> (E b, a)  -> G a -> G a 
op f (E i w, x) = split i (f w x) 

-- glass model collision parameter
data W  = OO  -- shape is presented laid down and the first cut is vertical
        | OV  -- shape is presented laid down and first cut is horizontal
        | VO  -- shape is presented tall and first cut is vertical
        | VV  -- .......
         deriving (Show,Enum)

-- glass model glass element
type Glass = (Double, Double)

-- glass model collision function
rf :: T Glass W 
rf OO (x,y) (a,b) = ((a-x,b),(x,b-y))
rf OV (x,y) (a,b) = ((a-x,y),(a,b-y))
rf VO (y,x) (a,b) = ((a-x,b),(x,b-y))
rf VV (y,x) (a,b) = ((a-x,y),(a,b-y))

rs = [OO, OV, VO, VV]

enumW :: [[[W]]] 
enumW = [[]]:map g enumW where
        g :: [[W]] -> [[W]]
        g xs = do
             x <- xs
             r <- rs
             return $ r:x

enumSol :: Int -> [[E W]]
enumSol n = do 
        (xs,ys) <- zip (tail $ enumW) (map (map reverse . fst) $ enumTa n)
        x <- xs
        y <- ys
        return $ zipWith E y x  

solve ::  [Glass] -> [Glass] -> [[[Glass]]]
solve mag cs = let
        tmag = fromList (zip [0..]  mag)
        in  validsSolve . map (\s -> map elems $ streamT rf tmag (zip s cs)) . filterSolve cs  $ enumSol (size tmag)


filterSolve y = takeWhile (\x -> length x == ly) . dropWhile (\x -> length x < ly)  where
        ly = length y

validsSolve = filter (all (\(x,y) -> x * y >= 0) . concat)




parse [x,y] = (x,y)
main = do
	Right ls <- parseCSVFromFile "glass.csv"
	let 	(mg,filter ((==) 2 . length) -> rq) = break ((/= 2) . length)  $ qs
		qs =  map (map (fst . head)) . filter (all $ not . null) $ map (\l -> map (reads :: String -> [(Double,String)]) l) ls
	print (mg,rq)
	print . head $ solve (map parse mg) (map parse rq)


-- mapM_ print  $ (Prelude.filter (all (\(x,y) -> x * y >= 0) . concat)) $ enumerateSolve [(8,6)] [(3,4),(8,1),(1,5)] 
        


