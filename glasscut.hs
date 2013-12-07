
{-# LANGUAGE ScopedTypeVariables, ViewPatterns, Rank2Types #-}

import System.IO
import System.Random hiding (split)
import Text.CSV
import Control.Arrow (second)
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Data.List hiding (delete)
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Animate
import System.Environment
import Data.Ord
import Data.Function
import Shuffle

-- glass model glass element
type Glass = (Float, Float)
type Pos = (Float, Float)

type PGlass = (Pos,Glass)
type K a b = [(a,b)]

-- stepping structure
type Choice = (PGlass,PGlass)
type Cutter = (Glass,K Int (K W Choice))
type Cuts  = K Int Cutter
type Rests = K Int PGlass


-- glass model collision parameter
data W  = OO  -- shape is presented laid down and the first cut is vertical
        | OV  -- shape is presented laid down and first cut is horizontal
        | VO  -- shape is presented tall and first cut is vertical
        | VV  -- .......
         deriving (Show,Enum)

-- glass model collision function
rf OO (x,y) ((px,py),(a,b)) = (OO, (((px + x,py),(a-x,b)),((px,py + y),(x,b-y))))
rf OV (x,y) ((px,py), (a,b)) = (OV, (((px + x,py),(a-x,y)),((px,py + y),(a,b-y))))
rf VO (x,y) ((px,py), (a,b)) = (VO, (((px + y,py),(a-y,b)),((px,py + x),(y,b-x))))
rf VV (x,y) ((px,py), (a,b)) = (VV, (((px + y,py),(a-y,x)),((px,py + x),(a,b-x))))

testInc :: Glass -> PGlass -> K W (PGlass,PGlass)
testInc c@(x,y) r@(_,(a,b)) = (if a >= x && b >= y then [rf OO c r,rf OV c r] else []) ++ (if b >= x && a >= y then [rf VO c r,rf VV c r] else [])

data C 	= 	A (Int,PGlass)
	|	D Int

delete :: Eq a => a -> K a b -> K a b
delete i [] = []
delete i ((j,x):xs) 
	| i == j = xs
	| otherwise = (j,x):delete i xs

select i [] = error "index missing"
select i ((j,x):xs) 
	| i == j = x
	| otherwise = select i xs


update :: C -> Cutter -> Cutter
update (D i) (c,m) = (c,delete i m)
update (A (i,g)) (c,m) = (c, (i,testInc c g):m)

cut :: Int -> (PGlass,PGlass) -> Rests -> (Rests,((Int,PGlass),(Int,PGlass)))
cut j (g1,g2) m = ((lk2,g2):(lk1,g1): delete j m, ((lk1,g1),(lk2,g2))) where
                lk1  = (maximum . map fst $ m) + 1
		lk2 = lk1 + 1

correct :: Int -> (Int,PGlass) -> (Int,PGlass) -> Cutter -> Cutter
correct i jg1 jg2 c =  foldr update c [A jg1,A jg2,D i]

type Pick m a b = K b a  -> m (b,a)

step :: Cuts -> Rests -> IO ((W,(Int,Glass),PGlass),(Cuts, Rests))
step  cs rs = do
	cs' <- shuffle cs
        let cs'' = sortBy (comparing  (\(_,(_,p)) -> length p)) cs'
	(i,(g,ct)) 	<- return $ head  cs''
	(j,cw) 		<- choiceIO ct
	(w,(g1,g2)) 	<- choiceIO  cw
	let  (rs',(ig1,ig2)) 	= cut j (g1,g2) rs
	     cs' 		= map (second $ correct j ig1 ig2) (delete i cs)
	     g' 		= select j rs
	return ((w,(i,g),g'),(cs',rs'))
	
---------------------------------------------------------------------------------------------------------
	
parse [x,y] = (x,y)

mkRests :: [[Float]] -> Rests
mkRests = zip [0..] . snd . mapAccumL (\dx (x,y) -> (dx + x,((dx,0),(x,y)))) 0 . map parse

mkCuts :: Rests -> [[Float]] -> Cuts
mkCuts rs = zip [0..] . map (\x -> foldr (update . A) (parse x,[]) rs)


main = do
	ls <- getArgs
	let frames = read  $ head ls
	Right ls <- parseCSVFromFile "glass.csv"
	let 	(mg,filter ((==) 2 . length) -> rq) = break ((/= 2) . length)  $ qs
		qs =  map (map (fst . head)) . filter (all $ not . null) $ map (\l -> map (reads :: String -> [(Float,String)]) l) ls
	let 	mg' = mkRests mg	
		rq' = mkCuts mg' rq
	
	let r rq mg ds = do 
		let rq' = filter (\(_,(_,t)) -> not $ null t) $ map (\(n,(g,r)) -> (n,(g, filter (\(_,t) -> not $ null t) r))) rq
		case rq' of 
			[] -> return ds
			rq' -> do	
				(d,(rq'',mg')) <- step rq' mg 
				r rq'' mg' $ d:ds

	best <- newTVarIO []
	current <- newTVarIO []
	let br r l h = pictures [
		color (makeColor r 0.5 0.5 1) $ rectangleSolid l h,
		rectangleWire l h 
		]
        let rend r = scale 100 100 . pictures .
					map (\(w,(_,(l,h)),((x,y),_)) -> case w of
						OO -> translate (x + l/2) (y + h/2) $ br r l h 
						OV -> translate (x + l/2) (y + h/2) $ br r l h 
						VO -> translate (x + h/2) (y + l/2) $ br r h l
						VV -> translate (x + h/2) (y + l/2) $ br r h l
						) 
	let pat = do
		a <- fmap (rend 0.8) . atomically $ readTVar best
		b <- fmap (rend 0.4) . atomically $ readTVar current
		return $ pictures [a,translate 0 400 b]
	let area = foldr (\(_,(_,(x,y)),_) a -> a + (x*y)) 0

	let cy n k = do
		ds <- r rq' mg' [] 
		let n' = area ds
		putStr (show k ++ "\r") 
		hFlush stdout
		case n' >= n of 
			True ->  do 
				putStr "\n" 
				print n'
				atomically (writeTVar best ds)
				cy n' (k + 1)
			False -> do 
				when (k `mod` frames == 0) $ atomically (writeTVar current ds)
				cy n (k + 1)
	forkIO $ cy 0 0
	animateIO (InWindow "glasscut"  (1300,300) (0,0)) (makeColor 1 1 1 1) $ (\_ -> pat)
        


