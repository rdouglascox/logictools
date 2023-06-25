module LogicTools.Data.GPLIProp (Prop (..), Predicate (..), Term (..)) where

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
