
-- stepping structure
type G a = Map Int a 

-- given and index to an element and a splitting function eliminate the element splitting it in two new ones
split :: Int -> (a -> (a,a)) -> G a -> G a
split i f m = insert i x . insert (lk m + 1) y $ m where
                (x,y) = f (m ! i)
                lk  = fst . findMax 

pickandsplit :: (a -> a -> (a,a)) -> Int -> Int -> G a -> G a -> (G a,G a)
pickandsplit f i j mi mj  = (delete i mi,split j (f (mi ! i)) mj) 

-- glass model glass element
type Glass = (Double, Double)

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

data Cut a = Cut 
	{	mis :: Glass
	,	[(Int,W)]
	}

testInc (x,y) (a,b) = (if a >= x and b >= y then [OO,OV]) ++ (if b >= x and a >= y then [VO,VV])
part = partition (\(n,i) -> length n == 0) . map (\i -> (testInc (mi ! i)) [elems mj],i) $ 

pickcutandsplit :: W -> (Int,Int)  -> G a -> G a -> (G a,G a)
pickcutandsplit (i,j) = pickandsplit (rf w) i j


test :: G a -> Bool
test m = Map.all (\(x,y) -> x > 0 && y > 0)

testM (m,_) = if test m then Just m else Nothing

rs = [OO, OV, VO, VV]


cut ::  (G a, G a) -> (Int,Int,W) -> Maybe (G a,G a)
pester ::  (G a, G a) -> (Int,Int) -> Either Int (G a,G a)
pester (mg,ct) x = let
	mg' = split 
run :: S -> 
