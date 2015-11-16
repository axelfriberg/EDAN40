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
-- numFactors :: Int -> Int
-- numFactors n =
	--length . filter 0 (map (mod n) [1..n])

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
multiply :: Num a => [a] -> a
multiply [a] =
	| length [a] == 0 = 0
	| otherwise 