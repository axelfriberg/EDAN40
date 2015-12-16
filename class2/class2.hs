-- Propositional Logic
-- Design a data type Proposition to represent propositions.
{- 
A proposition is a boolean formula of one of the following forms:
• a variable name (a string)
• p ∧ q (and)
• p ∨ q (or)
• ¬p (not)
where p and q are propositions. For example, p ∨ ¬p is a proposition.
-}

-- 1. Design a data type Proposition to represent propositions.
data Proposition = Var String | And Proposition Proposition | Or Proposition Proposition | Not Proposition deriving ( Eq, Show )

{-
2. Define a function

vars :: Proposition -> [String]

which returns a list of the variables in a proposition. Make sure each
variable appears only once in the list you return.
-}

vars :: Proposition -> [String]
vars (Var x) = [x]
vars (And p1 p2) = vars p1 ++ vars p2
vars (Or p1 p2) = vars p2 ++ vars p2
vars (Not p) = vars p

{-
Suppose you are given a list of variable names and their values, of type
Bool, for example, [("p",True),("q",False)]. 

Define a function

truthValue :: Proposition -> [(String,Bool)] -> Bool

which determines whether the proposition is true when the variables have
the values given.
-}
truthValue :: Proposition -> [(String,Bool)] -> Bool
truthValue (Var x) values = 
	case lookup x values of
    Just(a) -> a
    Nothing -> False
truthValue (And p1 p2) values = truthValue p1 values && truthValue p2 values
truthValue (Or p1 p2) values = truthValue p1 values || truthValue p2 values
truthValue (Not p) values = not $ truthValue p values

{- 
3. Define a function
tautology :: Proposition -> Bool
which returns true if the proposition holds for all values of the variables
appearing in it
-}

--------------------------------------------------------------------------

{-
2.2
File Systems (TDA555)
A file either contains data or is a directory. A directory contains other files
(which may themselves be directories) along with a name for each one.

1. Design a data type to represent the contents of a directory. Ignore the
contents of files: you are just trying to represent file names and the way
they are organised into directories here.

2. Define a function to search for a given file name in a directory. You should
return a path leading to a file with the given name. Thus if your directory
contains a, b, and c, and b is a directory containing x and y, then searching
for x should produce b/x.
-}

data File
  = File String
  | Dir String [File]
 deriving ( Eq, Show )

type FileSystem = [File]

search :: String -> FileSystem -> String
search "" _ = ""
search _ [] = ""
{- 
search a (f:fs) 
  | a == f = a
  | otherwise = s ++ "/" ++ search a fs-}

exampleFileSystem :: FileSystem
exampleFileSystem =
  [ File "apa"
  , Dir "bepa" [ File "apa", Dir "bepa" [], Dir "cepa" [ File "bepa" ] ]
  , Dir "cepa" [ Dir "bepa" [], Dir "cepa" [ File "apa" ] ]
  ]

-----------------------------------------------------------------------------

{-
2.3
Sets (TDA555)
1. Design a datastructure for sets . I.e. there should be a type Set a, and a
number of functions for creating, combining, and investigating sets. There
should at least be a function to create an empty set, add an element to a
set, take the union of two sets, remove an element from the set, and check
if an element is in the set.

2. Now, implement the Set datastructure. You may use lists internally.

3. Redo the above exercise, but now use sorted lists of unique elements as
your internal representation. Set union becomes more efficient that way.
-}

data Set a = Set [a]

createEmptySet :: Set a
createEmptySet = Set []

addElement :: Set a -> a -> Set a
addElement (Set xs) a = Set (a:xs)

union :: Set a -> Set a -> Set a
union (Set xs) (Set ys) = Set (xs ++ ys)

{-
2.4
Ordering (Thompson)
Complete the following instance declarations:
instance (Ord a, Ord b) => Ord (a,b) where ...
instance Ord b => Ord [b] where ...
where pairs and lists should be ordered lexicographically, like the words in dictionary.
-}

{-
2.5
ListNatural (lecture)
Natural numbers may correspond to lists of nothing!!
type ListNatural = [()]
For example:
twoL = [(),()]
threeL = [(),(),()]
What is: (:)
What is: (++)
What is: map (const ())
1. What do these functions do?
f1 x y = foldr (:) x y
f2 x y = foldr (const (f1 x)) [] y
f3 x y = foldr (const (f2 x)) [()] y
2. Continue this definition:
instance Num ListNatural where ...
Note: This requires ListNatural to be declared as a newtype 1 . One can
ask: Why?
-}

{-
2.6
Type derivation
Give the types of the following expressions:
(.) :: (b -> c) -> (a -> b) -> a -> c
(:) :: a -> [a] -> [a]

1. (.)(:) :: (a -> b) -> a -> [b] -> [b]
2. (:(.)) :: Type error
3. ((.):) :: 
4. ((:):)
5. Haskel wheels: (.)(.)
6. The Haskell smiley: (8-)
7. Haskell goggles: (+0).(0+)
8. A Haskell treasure: (($)$($))
9. Haskell swearing: ([]>>=)(\_->[(>=)])
-}