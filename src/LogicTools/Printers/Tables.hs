module LogicTools.Printers.Tables 
   (printtable,
    printboxes, 
    m2strings, 
    tb2strings) where 

import Text.PrettyPrint.Boxes 
import LogicTools.Tables.Tables 
import LogicTools.Data.PLProp
import Data.List

showBool :: Bool -> String 
showBool True = "T"
showBool False = "F"

mh2string :: [RawBasic] -> [String]
mh2string xs = map (\x->[x]) xs

bh2string :: [Prop] -> [String]
bh2string = map printprop 

joinheaders :: [RawBasic] -> [Prop] -> [String] 
joinheaders x y = mh2string x ++ bh2string y

m2strings :: [MatrixRow] -> [[String]]
m2strings = map (map showBool)

tb2strings :: [BodyRow] -> [[String]]
tb2strings = map (map showBool)

joinrows :: [[String]] -> [[String]] -> [[String]]
joinrows [] ys = []
joinrows (x:xs) (y:ys) = (x ++ y) : joinrows xs ys


-- printtable' :: Table -> [[String]]
-- printtable' (Table mh bh m tb) = (joinheaders mh bh) : (joinrows m tb) 

printtable :: Table -> [[String]]
printtable (Table mh bh m tb) = (joinheaders mh bh) : joinrows (m2strings m) (tb2strings tb)

-- toboxrows :: Table -> [Box]
-- toboxrows t = map (hsep 1 center1) (map (map text) (printtable t))


pad width x = x ++ replicate k ' '
   where k = width - length x

fmt_column :: [String] -> Box
fmt_column items = hsep // vcat left (intersperse hsep (map (text.pad width) items)) // hsep
   where width = maximum $ map length items
         hsep = text ( replicate width '-' )

table :: [[String]] -> Box
table rows = vsep Text.PrettyPrint.Boxes.<> hcat top (intersperse vsep (map fmt_column columns)) Text.PrettyPrint.Boxes.<> vsep
   where
     columns = transpose rows
     nrows = length rows
     vsep =  vcat left $ map char ("+" ++ (concat $ replicate nrows "|+")) 

printboxes :: Table -> String  
printboxes t = render $ table (printtable t)
