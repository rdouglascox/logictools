module LogicTools.Forms.Forms where

import LogicTools.Data.PLProp
import Data.List.Unique

-- | Data type for Logical Forms  
data Form =
     WffVar Int
   | NegationForm Form Form 
   | ConjunctionForm Form Form 
   | DisjunctionForm Form Form 
   | ConditionalForm Form Form 
   | BiconditionalForm Form Form 
   deriving (Show, Eq, Ord) 

-- | Enhanced data type for propositions including indexing
-- where propositions are identical to other subpropositions
-- of the proposition 
data IndexedProp = 
     IndexedBasic (Maybe Int) RawBasic 
   | IndexedTop (Maybe Int)   
   | IndexedBottom (Maybe Int) 
   | IndexedNegation (Maybe Int) Prop 
   | IndexedConjunction (Maybe Int) Prop Prop
   | IndexedDisjunction (Maybe Int) Prop Prop
   | IndexedConditional (Maybe Int) Prop Prop
   | IndexedBiconditional (Maybe Int) Prop Prop

-- | Get all the subformulas of a formula
subformulas :: Prop -> [Prop] 
subformulas (Basic x) = [(Basic x)] 
subformulas Top = [Top]
subformulas Bottom = [Bottom]
subformulas (Negation x) = Negation x : subformulas x
subformulas (Conjunction x y) = Conjunction x y : (subformulas x ++ subformulas y)
subformulas (Disjunction x y) = Disjunction x y : (subformulas x ++ subformulas y)
subformulas (Conditional x y) = Conditional x y : (subformulas x ++ subformulas y)
subformulas (Biconditional x y) = Biconditional x y : (subformulas x ++ subformulas y)

-- | We want to get a list of duplicate props from the
-- subformulas and then index them

duplicates :: [Prop] -> [[Prop]] 
duplicates xs = filter (\x -> length x > 1) (group xs)  


