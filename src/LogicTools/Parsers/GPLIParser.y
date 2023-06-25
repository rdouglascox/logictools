{
module LogicTools.Parsers.GPLIParser (parser, parseGPLI) where
import LogicTools.Parsers.GPLIToken
import LogicTools.Data.GPLIProp
}

%name parser
%tokentype { GPLIToken }
%error { parseError }
%monad {Maybe}

%token
    pred         { PredicateSymbol $$ }
    const        { ConstantSymbol $$ }
    vari         { VariableSymbol $$ }
    "~"          { NegationSymbol }
    "&"          { ConjunctionSymbol }
    "v"          { DisjunctionSymbol }
    "->"         { ConditionalSymbol }
    "<->"        { BiconditionalSymbol }
    "@"          { UniversalSymbol }
    "#"          { ExistentialSymbol }
    "("          { LeftPar }
    ")"          { RightPar }

%%

prop : pred terms                { Atomic (Predicate $1) $2  }
     | "~" prop                  { Negation $2 } 
     | "(" prop "&" prop ")"     { Conjunction $2 $4 }  
     | "(" prop "v" prop ")"     { Disjunction $2 $4 }
     | "(" prop "->" prop ")"    { Conditional $2 $4 }
     | "(" prop "<->" prop ")"   { Biconditional $2 $4 }
     | "@" vari prop             { Universal $2 $3 }
     | "#" vari prop             { Existential $2 $3 }

terms : const                { [Constant $1] }
      | vari                 { [Variable $1] }      
      | const terms          { Constant $1 : $2 }
      | vari terms           { Variable $1 : $2 }


{

parseGPLI :: String -> Maybe Prop
parseGPLI x = parser $ alexScanTokens x

parseError :: [GPLIToken] -> a
parseError _ = error "Parse Error"

}


