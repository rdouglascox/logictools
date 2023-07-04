
{- | 
Module : Tables 
Copyright: (c) Ryan Cox, 2023
License: GPL-3 

This module contains functions for building truth tables as well as associated functions.

 -}

module LogicTools.Tables.Tables 
     -- * Truth Tables 
     -- |
    (MatrixRow 
    , BodyRow 
    , Table(..)
    -- * Constructing Tables
    , Assignment
    , maketable
    , getbasics
    , getbasicsfrmlst
    , makeassignments
    , makematrix
    , eval
    , maketablebody
      -- * Checking tables
    , somerow 
    , allrows
    , equiv 
    , validity
    , taut 
      -- * Other 
    , tdnf
    , simplifytdnf) where 

import LogicTools.Data.PLProp
import Data.List
import qualified Data.Map as Map

-- | Data declaration for truth tables
data Table = Table { matrixheader :: [RawBasic] -- ^ Matrix Header
                   , bodyheader :: [Prop]  -- ^ Body Header
                   , matrix :: [MatrixRow] -- ^ Matrix 
                   , tablebody :: [BodyRow] -- ^ Table Body 
                   }
    deriving (Show) 

-- | `MatrixRow` is a type synonym for a list of booleans
-- that make up a row of the matrix of a table
type MatrixRow = [Bool]

-- | `BodyRow` is a type synonym for  a list of booleans
-- that make up a row of the body of a table
type BodyRow = [Bool]

-- | Build a truth table from a list of PL propositions
maketable :: [Prop] -> Table   
maketable xs = Table {matrixheader = thebasics
                     , bodyheader = xs 
                     , matrix = thematrix 
                     , tablebody = maketablebody thebasics xs 
                     }
               where thebasics = getbasicsfrmlst xs 
                     thematrix = makematrix thebasics 

-- | Get a list of basic propositions from a list of propositions  
getbasicsfrmlst :: [Prop] -> [RawBasic]
getbasicsfrmlst xs = sort $ nub $ (concatMap getbasics xs) 

-- | Get a list of basic propositions from a proposition 
getbasics :: Prop -> [RawBasic]
getbasics Top = []
getbasics Bottom = []
getbasics (Basic x) = [x]
getbasics (Negation x) = getbasics x
getbasics (Conjunction x y) = getbasics x ++ getbasics y  
getbasics (Disjunction x y) = getbasics x ++ getbasics y  
getbasics (Conditional x y) = getbasics x ++ getbasics y  
getbasics (Biconditional x y) = getbasics x ++ getbasics y  

type Assignment = Map.Map RawBasic Bool

allpossibilities :: [RawBasic] -> [[(RawBasic,Bool)]]
allpossibilities xs = map (\x -> [(x,True),(x,False)]) xs

-- | Build all possible assignments for basic propositions.
-- This function is used build the matrix and the table
-- body.
makeassignments :: [RawBasic] -> [Assignment] 
makeassignments m = map Map.fromList (sequenceA (allpossibilities m))

-- | Build a matrix from a matrix header
makematrix :: [RawBasic] -> [MatrixRow]
makematrix mh = map Map.elems (makeassignments mh)  

-- | Evaluate a Proposition on an Assignment
eval :: Assignment -> Prop -> Bool 
eval _ Top = True 
eval _ Bottom = False 
eval a (Basic x) = a Map.! x   
eval a (Negation x) = not (eval a x)
eval a (Conjunction x y) = (eval a x) && (eval a y) 
eval a (Disjunction x y) = (eval a x) || (eval a y) 
eval a (Conditional x y) = (not (eval a x)) || (eval a y) 
eval a (Biconditional x y) = ((eval a x) && (eval a y)) || (not (eval a x) && not (eval a y)) 

-- | Evaluate a Proposition on a list of assignments 
evals :: [Prop] -> Assignment -> [Bool] 
evals p as = map (eval as) p

-- | Build a table body from a list of assignments and a
-- list of propositions 
maketablebody :: [RawBasic] -> [Prop] -> [BodyRow]
maketablebody mh bh = map (evals bh) (makeassignments mh)

-- | Build a table body just from the props
maketablebody' :: [Prop] -> [BodyRow] 
maketablebody' bh = maketablebody (getbasicsfrmlst bh) bh 

-- | Test for all true on some row
somerow :: [Prop] -> Bool 
somerow xs = or (map and (maketablebody' xs))

-- | Test for (all true) on every row 
allrows :: [Prop] -> Bool 
allrows xs = and (map and (maketablebody' xs))

-- | Test for tautology
taut :: Prop -> Bool
taut p = all (\x -> (eval x p)) (makeassignments (getbasics p))

-- | Test for no row on which the premises are true and the
-- conclusion false 
validity :: [Prop] -> Bool
validity xs = not $ any checkrow (maketablebody' xs)

checkrow :: [Bool] -> Bool 
checkrow ys = case reverse ys of  
                [] -> False 
                (x:xs) -> (x == False) && (and xs)    

-- | Test for equivalence 
equiv :: [Prop] -> Bool 
equiv xs = and (map (\x -> (all (==True) x || all (==False) x)) (maketablebody' xs)) 

-- | Disjunctive Normal Form from Tables 
tdnf :: Prop -> Prop 
tdnf p = let rawbasics = getbasics p 
             assignments = makeassignments rawbasics 
             truerows = filter (\x -> (eval x p == True)) assignments
             allrows = map readrow truerows 
             asconjunctions = map (\x -> foldr Conjunction Top x) allrows 
             in foldr Disjunction Bottom asconjunctions             
 
readrow :: Assignment -> [Prop] 
readrow p = let (pos,neg) = Map.partition (==True) p in 
            map Basic (Map.keys pos) ++ map (Negation . Basic) (Map.keys neg) 

-- | Simply Table Disjunctive Normal Form
simplifytdnf :: Prop -> Prop 
simplifytdnf (Conjunction x (Conjunction y Top)) = Conjunction (simplifytdnf x) (simplifytdnf y)
simplifytdnf (Disjunction x (Disjunction y Bottom)) = Disjunction (simplifytdnf x) (simplifytdnf y)
simplifytdnf (Disjunction x Bottom) = simplifytdnf x
simplifytdnf (Conjunction x Top) = simplifytdnf x
simplifytdnf (Conjunction x y) = Conjunction (simplifytdnf x) (simplifytdnf y)
simplifytdnf (Disjunction x y) = Disjunction (simplifytdnf x) (simplifytdnf y)
simplifytdnf x = x
