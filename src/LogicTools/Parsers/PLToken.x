{
module LogicTools.Parsers.PLToken where
}

%wrapper "basic"

$alpha = [A-Z]

tokens :-

  $white+                             ;
  $alpha                              { \s -> BasicSymbol s }
  "~"                                 { \s -> NegationSymbol }
  "&"                                 { \s -> ConjunctionSymbol }
  "v"                                 { \s -> DisjunctionSymbol }
  "->"                                { \s -> ConditionalSymbol }
  "<->"                               { \s -> BiconditionalSymbol }
  "("                                 { \s -> LeftPar }
  ")"                                 { \s -> RightPar }
  
{

data PLToken = BasicSymbol String 
               | NegationSymbol
               | ConjunctionSymbol
               | DisjunctionSymbol
               | ConditionalSymbol
               | BiconditionalSymbol
               | LeftPar
               | RightPar
               deriving (Eq,Show)

} 
