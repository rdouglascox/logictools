{
module LogicTools.Parsers.PLParser (parsePL) where
import LogicTools.Parsers.PLToken
import LogicTools.Data.PLProp (Prop (..))
}

%name parser
%tokentype { PLToken }
%error { parseError }
%monad {Maybe}

%token
    basic        { BasicSymbol $$ }
    "~"          { NegationSymbol }
    "&"          { ConjunctionSymbol }
    "v"          { DisjunctionSymbol }
    "->"         { ConditionalSymbol }
    "<->"        { BiconditionalSymbol }
    "("          { LeftPar }
    ")"          { RightPar }

%%

Prop : basic         { Basic (head $1) }
     | "~" Prop                  { Negation $2 } 
     | "(" Prop "&" Prop ")"     { Conjunction $2 $4 }  
     | "(" Prop "v" Prop ")"     { Disjunction $2 $4 }
     | "(" Prop "->" Prop ")"    { Conditional $2 $4 }
     | "(" Prop "<->" Prop ")"   { Biconditional $2 $4 }

{

parsePL :: String -> Maybe Prop 
parsePL x = parser $ alexScanTokens x

parseError :: [PLToken] -> Maybe a
parseError _ = Nothing

}

