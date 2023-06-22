{-# LANGUAGE OverloadedStrings #-}

module LogicTools.Printers.LaTeXTables (makeTable) where

import Text.LaTeX.Base
import Text.LaTeX.Base.Commands
import Text.LaTeX.Base.Class
import Text.LaTeX.Base.Math
import LogicTools.Printers.LaTeXPLProp
import LogicTools.Data.PLProp
import LogicTools.Tables.Tables
import Data.List

-- CenterColumn, VerticalLine, DVerticalLine

makeTable :: Table -> LaTeX
makeTable (Table mh bh m b) = tabular Nothing spec content
    where spec = makeSpec (mh,bh)
          content = hline <> makeHeaderRow (mh,bh) <> hline <> makeBodyRows (m,b) <> hline

makeSpec :: ([Char],[Prop]) -> [TableSpec]
makeSpec (mh,bh) = [VerticalLine] <> (intersperse VerticalLine [CenterColumn | x <- mh]) <> [DVerticalLine] <> (intersperse VerticalLine [CenterColumn | x <- bh]) <> [VerticalLine]

makeHeaderRow :: ([Char],[Prop]) -> LaTeX
makeHeaderRow (mh,bh) = foldl1 (&) (map proptoLaTeX ((map Basic mh) ++ bh)) <> tabularnewline   

unify [] [] = []
unify (x:xs) (y:ys) = (x ++ y): unify xs ys 

makeBodyRows :: ([[Bool]],[[Bool]]) -> LaTeX
makeBodyRows (m,b) = mconcat $ map makeRow (unify m b)

makeRow :: [Bool] -> LaTeX 
makeRow bs = foldl1 (&) (map printBool bs) <> tabularnewline 

printBool :: Bool -> LaTeX
printBool True = fromString "T"
printBool False = fromString "F" 
