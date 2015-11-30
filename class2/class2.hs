-- Propositional Logic
-- Design a data type Proposition to represent propositions.
{- 
A proposition is a boolean formula of one of the following forms:
• a variable name (a string)
• p ∧ q (and)
• p ∨ q (or)
• ¬p (not)
where p and q are propositions. For example, p ∨ ¬p is a proposition.

1. Design a data type Proposition to represent propositions.

2. Define a function
vars :: Proposition -> [String]
which returns a list of the variables in a proposition. Make sure each
variable appears only once in the list you return.
Suppose you are given a list of variable names and their values, of type
Bool, for example, [("p",True),("q",False)]. Define a function
truthValue :: Proposition -> [(String,Bool)] -> Bool
which determines whether the proposition is true when the variables have
the values given.

3. Define a function
tautology :: Proposition -> Bool
which returns true if the proposition holds for all values of the variables
appearing in it.

-}
data Proposition = String | Proposition :&: Proposition | Proposition :|: Proposition | Not Proposition deriving ( Eq, Show )


{- 
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

search :: FileSystem -> String -> [String]
search [] _ = []
search _ "" = []
search files name = map (file==)

{- 
Give the types of the following expressions:
1. (.)(:)
2. (:(.))
3. ((.):)
4. ((:):)
5. Haskel wheels: (.)(.)
6. The Haskell smiley: (8-)
7. Haskell goggles: (+0).(0+)
8. A Haskell treasure: (($)$($))
9. Haskell swearing: ([]>>=)(\_->[(>=)])

1. 
2. 
3. 
4.
5. 
6. (8-) :: Num a => a -> a
7. (+0).(0+) :: Num a => 
8.
9.
-}
