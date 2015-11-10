-- Class 1

-- maxi x y
-- The first row below reads: The function maxi takes any two values that are of the same type and returns a the same type. 
-- The type of those two values must be a member of the Ord class which is for types that have an ordering (this was the class constraint).
maxi :: Ord a => a -> a -> a
maxi x y = 
	if x > y
	then x
	else y