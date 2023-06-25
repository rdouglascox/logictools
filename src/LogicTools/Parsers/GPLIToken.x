{
module LogicTools.Parsers.GPLIToken where
}

%wrapper "basic"

$upper = A-Z
$lower1 = a-t
$lower2 = x-z

tokens :-

  $white+                             ;
  $upper                              { \s -> PredicateSymbol (head s) }
  $lower1                             { \s -> ConstantSymbol (head s) }
  $lower2                             { \s -> VariableSymbol (head s) }
  "~"                                 { \s -> NegationSymbol }
  "&"                                 { \s -> ConjunctionSymbol }
  "v"                                 { \s -> DisjunctionSymbol }
  "->"                                { \s -> ConditionalSymbol }
  "<->"                               { \s -> BiconditionalSymbol }
  "@"                                 { \s -> UniversalSymbol }
  "#"                                 { \s -> ExistentialSymbol }
  "("                                 { \s -> LeftPar }
  ")"                                 { \s -> RightPar }
  
{

data GPLIToken = PredicateSymbol Char
               | VariableSymbol Char
               | ConstantSymbol Char
               | NegationSymbol
               | ConjunctionSymbol
               | DisjunctionSymbol
               | ConditionalSymbol
               | BiconditionalSymbol
               | UniversalSymbol
               | ExistentialSymbol
               | LeftPar
               | RightPar
               deriving (Eq,Show)

} 
