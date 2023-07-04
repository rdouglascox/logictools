module LogicTools.Data.GPLIProp (Prop (..), Predicate (..), Term (..), printprop, printprops) where

import Data.List

data Prop = Atomic Predicate [Term]
          | Negation Prop
          | Existential Char Prop
          | Universal Char Prop
          | Conjunction Prop Prop
          | Disjunction Prop Prop
          | Conditional Prop Prop
          | Biconditional Prop Prop
          deriving (Show,Eq,Ord)

data Predicate = Predicate Char
    deriving (Show,Eq,Ord)

data Term = Variable Char
          | Constant Char
          deriving (Show, Eq,Ord)

printprop = printProp

printProp :: Prop -> String
printProp (Atomic (Predicate x) xs) = x : printTerms xs
printProp (Negation l) = "~" ++ printProp l
printProp (Conjunction l r) = "(" ++ printProp l ++ "&" ++ printProp r ++ ")"
printProp (Disjunction l r) = "(" ++ printProp l ++ "v" ++ printProp r ++ ")"
printProp (Conditional l r) = "(" ++ printProp l ++ "->" ++ printProp r ++ ")"
printProp (Biconditional l r) = "(" ++ printProp l ++ "<->" ++ printProp r ++ ")"
printProp (Universal x p) = "@" ++ [x] ++ printProp p
printProp (Existential x p) = "#" ++ [x] ++ printProp p

printTerms :: [Term] -> String
printTerms [] = []
printTerms (Variable x:xs) = x : printTerms xs
printTerms (Constant x:xs) = x : printTerms xs

printprops :: [Prop] -> String
printprops ps = intercalate ", " (map printProp ps)
