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
