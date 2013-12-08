
{-# LANGUAGE MultiParamTypeClasses, ScopedTypeVariables, ViewPatterns, Rank2Types, FlexibleInstances #-}

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
import Data.Monoid

-- glass model glass element
type Glass = (Float, Float)
type Pos = (Float, Float)

type PGlass = (Pos,Glass)
type K a b = [(a,b)]
data K' a b = K' Int (K a b)

instance Monoid (K' a b) where
	K' i x `mappend` K' j y = K' (i  + j) (x ++ y)
	mempty = K' 0 []

choiceIO (K' i xs) = (xs !!) `fmap` randomRIO (0,i - 1)

-- stepping structure
type Choice = (PGlass,PGlass)
type Cutter = (Glass,K' Int (K' W Choice))
type Cuts  = K' Int Cutter
type Rests = K' Int PGlass



-- glass model collision parameter
data W  = OO  -- shape is presented laid down and the first cut is vertical
        | OV  -- shape is presented laid down and first cut is horizontal
        | VO  -- shape is presented tall and first cut is vertical
        | VV  -- .......
         deriving (Show,Enum,Eq)

-- glass model collision function
rf :: W -> Glass -> PGlass -> (W,Choice)
rf OO (x,y) ((px,py), (a,b)) = (OO, (((px + x,py),(a-x,b)),((px,py + y),(x,b-y))))
rf OV (x,y) ((px,py), (a,b)) = (OV, (((px + x,py),(a-x,y)),((px,py + y),(a,b-y))))
rf VO (x,y) ((px,py), (a,b)) = (VO, (((px + y,py),(a-y,b)),((px,py + x),(y,b-x))))
rf VV (x,y) ((px,py), (a,b)) = (VV, (((px + y,py),(a-y,x)),((px,py + x),(a,b-x))))



testInc :: Glass -> PGlass -> K' W (PGlass,PGlass)
testInc c@(x,y) r@(_,(a,b)) = 
		(if a >= x && b >= y then K' 2 [rf OO c r,rf OV c r] else mempty) 
			`mappend` 
		(if b >= x && a >= y then K' 2 [rf VO c r,rf VV c r] else mempty)

data C 	= 	A (Int,PGlass)
	|	D Int

consK x (K' n xs) = K' (n + 1) (x:xs)

delete :: Eq a => a -> K' a b -> K' a b
delete i (K' _ []) = mempty
delete i (K' n ((j,x):xs)) 
	| i == j = K' (n - 1) xs
	| otherwise = (j,x) `consK` delete i (K' (n - 1) xs)

select i (K' _ []) = error "index missing"
select i (K' n ((j,x):xs))
	| i == j = x
	| otherwise = select i (K' (n-1) xs)


update :: C -> Cutter -> Cutter
update (D i) (c,m) = (c,delete i m)
update (A (i,g)) (c,m) = (c, (i,testInc c g) `consK` m)

cut :: Int -> (PGlass,PGlass) -> Rests -> (Rests,((Int,PGlass),(Int,PGlass)))
cut j (g1,g2) m@(K' _ zu)  = ((lk2,g2) `consK` ((lk1,g1) `consK`  delete j m), ((lk1,g1),(lk2,g2))) where
                lk1  = (maximum . map fst $ zu) + 1
		lk2 = lk1 + 1


correct :: Int -> (Int,PGlass) -> (Int,PGlass) -> Cutter -> Cutter
correct i jg1 jg2 c =  foldr update c [A jg1,A jg2,D i]

mapK f (K' i xs) = K' i (map f xs)

step :: Cuts -> Rests -> IO ((W,(Int,Glass),PGlass),(Cuts, Rests))
step  cs@(K' ncs csl)  rs = do
        let cs'' =  sortBy (comparing  (\(_,(_,K' p _)) -> p)) csl
	let (i,(g,ct)) = head cs''
	(j,cw) 		<- choiceIO ct
	(w,(g1,g2)) 	<- choiceIO cw
	let  (rs',(ig1,ig2)) 	= cut j (g1,g2) rs
	     cs' 		= mapK (second $ correct j ig1 ig2) (delete i cs)
	     g' 		= select j rs
	return ((w,(i,g),g'),(cs',rs'))
	
---------------------------------------------------------------------------------------------------------
	
parse [x,y] = (x,y)

mkRests :: [[Float]] -> Rests
mkRests ys = let 
	xs = zip [0..] . snd . mapAccumL (\dx (x,y) -> (dx + x,((dx,0),(x,y)))) 0 . map parse $ ys
	in K' (length xs) xs

mkCuts :: Rests -> [[Float]] -> Cuts
mkCuts (K' _ rs) ys = let 
	xs = zip [0..] . map (\x -> foldr (update . A) (parse x,mempty) rs) $ ys
	in K' (length xs) xs

filterK c (K' _ []) = mempty
filterK c (K' n (x:xs)) 
	| c x = x `consK` filterK c (K' (n - 1) xs)
	| otherwise = filterK c (K' (n - 1) xs)
nullK (K' _ []) = True
nullK _ = False

main = do
	ls <- getArgs
	let frames = read  $ head ls
	Right ls <- parseCSVFromFile "glass.csv"
	let 	(mg,filter ((==) 2 . length) -> rq) = break ((/= 2) . length)  $ qs
		qs =  map (map (fst . head)) . filter (all $ not . null) $ map (\l -> map (reads :: String -> [(Float,String)]) l) ls
	let 	mg' = mkRests mg	
		rq' = mkCuts mg' rq
	
	let r rq mg ds = do 
		let rq' = filterK (\(_,(_,t)) -> not $ nullK t) $ mapK (\(n,(g,r)) -> (n,(g, filterK (\(_,t) -> not $ nullK t) r))) rq
		case rq' of 
			(K' _ []) -> return ds
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
	let ofirst = sum . zipWith (\i (w,_,_) -> if w `elem` [OV,VV] then 0 else i) [1..]  
	let shuffleK (K' n xs) = K' n `fmap` shuffle xs
	let cy n k = do
		rq'' <- shuffleK rq'
		ds <- r rq'' mg' [] 
		let 	n' = area ds 
			z' =  ofirst ds
		putStr (show k ++ "\r") 
		hFlush stdout
		case n'*z' >= n of 
			True ->  do 
				putStr "\n" 
				print (n',z')
				atomically (writeTVar best ds)
				cy (n'*z') (k + 1)
			False -> do 
				when (k `mod` frames == 0) $ atomically (writeTVar current ds)
				cy n (k + 1)
	forkIO $ cy 0 0
	animateIO (InWindow "glasscut"  (1300,300) (0,0)) (makeColor 1 1 1 1) $ (\_ -> pat)
        

