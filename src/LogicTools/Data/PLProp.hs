module LogicTools.Data.PLProp 
        (Prop (..) 
        , RawBasic
        , printprop 
        , printprops) where

-- |Data type for Propositional Logic (PL). 
data Prop = 
     Basic RawBasic
   | Top
   | Bottom
   | Negation Prop
   | Conjunction Prop Prop
   | Disjunction Prop Prop
   | Conditional Prop Prop 
   | Biconditional Prop Prop
   deriving (Show, Eq, Ord) 

-- | Type for Basics
type RawBasic = Char

-- | Print a Proposition
printprop :: Prop -> String 
printprop (Basic x) = [x]
printprop Top = "⊤"
printprop Bottom = "⊥"
printprop (Negation x) = '~': printprop x
printprop (Conjunction x y) = "(" ++ printprop x ++ "&" ++ printprop y ++ ")"
printprop (Disjunction x y) = "(" ++ printprop x ++ "v" ++ printprop y ++ ")"
printprop (Conditional x y) = "(" ++ printprop x ++ "->" ++ printprop y ++ ")"
printprop (Biconditional x y) = "(" ++ printprop x ++ "<->" ++ printprop y ++ ")"
-- | Print Propositions
printprops :: [Prop] -> [String]
printprops = map printprop
