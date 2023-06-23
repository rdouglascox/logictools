module LogicTools.Forms.Forms where

import LogicTools.Data.PLProp

-- | Data type for Logical Forms  
data Form =
     WffVar Int
   | NegationForm Form Form 
   | ConjunctionForm Form Form 
   | DisjunctionForm Form Form 
   | ConditionalForm Form Form 
   | BiconditionalForm Form Form 
   deriving (Show, Eq, Ord) 

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



