-- 2.1 Simple Definitions
------------------------------- SETS
newtype Set a = Set [a]

empty :: Set a
empty = Set []

sing :: a -> Set a
sing x = Set [x]

memSet :: (Eq a) => a -> Set a -> Bool
memSet _ (Set []) = False
memSet x (Set xs)
    | elem x xs = True
    | otherwise = False

{-
makeSet :: (Eq a) => [a] -> Set a
makeSet [] = empty
makeset (x:xs) = union (sing x) (makeSet xs)
-- etc
-- we need the obvious stuff:

union        :: Set a -> Set a -> Set a
unionMult    :: [ Set a ] -> Set a
intersection :: Set a -> Set a -> Set a
subSet       :: Set a -> Set a -> Bool
mapSet       :: (a -> b) -> Set a -> Set b
mapset f (Set xs) = makeSet (map f xs)
-}

-- now making it a monad:
instance Monad Set where
   return = sing
   (Set x) >>= f =  unionMult (map f x)

-- remember to verify the monad laws at this point!

-- 1. return x >>= f = (Set [x]) >>= f = unionMult (map f [x]) = unionMult [ (f x) ] = f x
-- 2. (Set [xs]) >>= return = unionMult (map return [xs]) = unionMult [ys] = Set [xs]
--       where [ys] is a list of Set [x] where x are elements of Set [xs]
-- and 3. associativity, left for the reader

data Tree a = Leaf a | Node (Tree a) (Tree a)

instance Monad Tree where
	return = Leaf
	(Leaf t) >>= f   = f t
   	(Node l r) >>= f = Node (l >>= f) (r >>= f)
   
------------------------------ ERROR

data Error a = OK a | Error String

instance Monad Error where
	return = OK
	(OK a) >>= f = f a
	(Error s) >>= f = (Error s)

----------------------------------------------------------------
-- 2.2. Monadic helper functions

-- We could use maybe?

onlyIfM :: Monad m => m Bool -> m () -> m ()
onlyIfM b m = do result <- b
                 if result 
                    then m
                    else return ()

----------------------------------------------------------------
-- 2.3. List comprehension

list1 = [ (x,y) | x <- [1..], y <- [1..x]]

list2 = do
  x <- [1..]
  y <- [1..x]
  return (x,y)

--list2' = x <- [1..] >>= y <- [1..x] >>= (x,y)

list2' = [1..] >>= (\x -> [1..x] >>= (\y -> return (x,y)))

