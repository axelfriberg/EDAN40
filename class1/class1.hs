-- Class 1

-- maxi x y
-- The first row below reads: The function maxi takes any two values that are of the same type and returns a the same type. 
-- The type of those two values must be a member of the Ord class which is for types that have an ordering (this was the class constraint).
maxi :: Ord a => a -> a -> a
maxi x y = 
	if x > y
	then x
	else y

-- sumsqr returns 1*1+2*2...+n*n
sumsqr 0 = 0
sumsqr n = n^2 + sumsqr (n-1)

sumsqm n = sum (map (^2) [1..n])

-- hanoi
hanoi 0 = 0
hanoi n = 1 + 2 * hanoi (n-1)

-- Factors
smallestFactor n = nextFactor 1 n

nextFactor k n 
	| k >= n            = n
	| mod n (k+1) == 0  = k+1
	| otherwise         = nextFactor (k+1) n

-- Why does't this funtion work?
numFactors :: Int -> Int
numFactors n =
	length $ filter (==0) $ (map (mod n) [1..n])

-- Defining types
type Month = Integer
daysInMonth :: Month -> Integer -> Integer
daysInMonth m y -- Why no equals here?
	| m <= 0 || m > 12   = 0
	| m == 1 || m == 3 || m == 5 || m == 7 || m == 8 || m == 10 || m == 12  = 31
	| m == 4 || m == 6 || m == 9 || m == 11   = 30
	| mod y 4 == 0  = 29
	| otherwise     = 28

data Date = Date Integer Month Integer -- Why do you need to write Date first after equals?
validDate :: Date -> Bool
validDate (Date d m y)
 	| (d > 0) && (d <= daysInMonth m y) = True
 	| otherwise                         = False 

 -- Multiplying list elements
 -- Why not multiply [a] = foldl1 (*) [a]?
multiply :: Num a => [a] -> a
multiply = foldl1 (*)

-- substitution
-- substitute :: Eq a => a -> a -> [a] -> [a]
-- Doesnt work, check solution

--duplicates
duplicates :: Eq a => [a] -> Bool
duplicates [] = False
duplicates [x] = False
duplicates (x:xs) 
 | elem x xs = True -- Does this not return as soon as it finds an x?
 | otherwise = duplicates xs

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates [x] = [x]
removeDuplicates (x:xs)
  | elem x xs  = removeDuplicates xs
  | otherwise  = x : removeDuplicates xs

prop_duplicatesRemoved :: [Integer] -> Bool
prop_duplicatesRemoved xs = not (duplicates (removeDuplicates xs))

-- Pairs matches combines the elements from two lists with the same index.
pyth n = [(a,b,c)|a<-[1..n],b<-[a..n],c<-[b..n],a^2 + b^2 == c^2]

mystery xs = foldr (++) [] (map (\y -> [y]) xs)