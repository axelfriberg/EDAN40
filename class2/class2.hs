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
data Proposition = Var String | And Proposition :&: Proposition | Or Proposition :|: Proposition | Not Proposition deriving ( Eq, Show )

{-
2. Define a function

vars :: Proposition -> [String]

which returns a list of the variables in a proposition. Make sure each
variable appears only once in the list you return.

Suppose you are given a list of variable names and their values, of type
Bool, for example, [("p",True),("q",False)]. 

Define a function

truthValue :: Proposition -> [(String,Bool)] -> Bool

which determines whether the proposition is true when the variables have
the values given.
-}

vars :: Proposition -> [String]
vars (Var x) = [x]
vars (And p1 :&: p2) = vars p1 ++ vars p2
vars (Or p1 :|: p2) = vars p2 ++ pars p2
vars (Not p) = vars p