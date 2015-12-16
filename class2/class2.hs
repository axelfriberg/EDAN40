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