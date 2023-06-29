module LogicTools.Forms.Forms (getforms) where

import LogicTools.Data.PLProp
import Data.List
import Data.Map as Map ((!), member, fromList, Map)

-- | Data type for Logical Forms  
data Form =
     BasicForm (Maybe Int) 
   | NegationForm (Maybe Int) (Maybe Form)
   | ConjunctionForm (Maybe Int) (Maybe Form) (Maybe Form)
   | DisjunctionForm (Maybe Int) (Maybe Form) (Maybe Form)
   | ConditionalForm (Maybe Int) (Maybe Form) (Maybe Form) 
   | BiconditionalForm (Maybe Int) (Maybe Form) (Maybe Form) 
   deriving (Show, Eq, Ord) 

-- | Enhanced data type for propositions including indexing
-- where propositions are identical to other subpropositions
-- of the proposition 
data IndexedProp = 
     IndexedBasic (Maybe Int) RawBasic 
   | IndexedTop (Maybe Int)   
   | IndexedBottom (Maybe Int) 
   | IndexedNegation (Maybe Int) IndexedProp 
   | IndexedConjunction (Maybe Int) IndexedProp IndexedProp
   | IndexedDisjunction (Maybe Int) IndexedProp IndexedProp
   | IndexedConditional (Maybe Int) IndexedProp IndexedProp
   | IndexedBiconditional (Maybe Int) IndexedProp IndexedProp
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

-- | We want to get a list of duplicate props from the
-- subformulas and then index them

duplicates :: [Prop] -> [Prop]
duplicates xs = nub (xs \\ (nub xs))

indexprop1 :: [Prop] -> Prop -> IndexedProp
indexprop1 ps (Basic x) = IndexedBasic (elemIndex (Basic x) ps) x    
indexprop1 ps (Negation x) = IndexedNegation (elemIndex (Negation x) ps) (indexprop1 ps x)
indexprop1 ps (Conjunction x y) = IndexedConjunction (elemIndex (Conjunction x y) ps) (indexprop1 ps x) (indexprop1 ps y) 
indexprop1 ps (Disjunction x y) = IndexedDisjunction (elemIndex (Disjunction x y) ps) (indexprop1 ps x) (indexprop1 ps y) 
indexprop1 ps (Conditional x y) = IndexedConditional (elemIndex (Conditional x y) ps) (indexprop1 ps x) (indexprop1 ps y) 
indexprop1 ps (Biconditional x y) = IndexedBiconditional (elemIndex (Biconditional x y) ps) (indexprop1 ps x) (indexprop1 ps y)
 
indexprop :: Prop -> IndexedProp 
indexprop x = indexprop1 (duplicates $ subformulas $ x) x

allforms :: IndexedProp -> [Form]
allforms (IndexedBasic im _) = [BasicForm im] 
allforms (IndexedNegation im x) = NegationForm im Nothing : NegationForm Nothing Nothing : map (NegationForm im) (map Just (allforms x)) ++ map (NegationForm Nothing) (map Just (allforms x))
allforms (IndexedConjunction im x y) =  
    ((ConjunctionForm im) <$> (map Just (allforms x)) <*> (map Just (allforms y))) ++
    ((ConjunctionForm im) <$> [Nothing] <*> (map Just (allforms y))) ++ 
    ((ConjunctionForm im) <$> (map Just (allforms x)) <*> [Nothing]) ++  
    ((ConjunctionForm im) <$> [Nothing] <*> [Nothing] ) ++ 
    ((ConjunctionForm Nothing) <$> (map Just (allforms x)) <*> (map Just (allforms y))) ++
    ((ConjunctionForm Nothing) <$> [Nothing] <*> (map Just (allforms y))) ++ 
    ((ConjunctionForm Nothing) <$> (map Just (allforms x)) <*> [Nothing]) ++  
    ((ConjunctionForm Nothing) <$> [Nothing] <*> [Nothing] )
allforms (IndexedDisjunction im x y) =  
    ((DisjunctionForm im) <$> (map Just (allforms x)) <*> (map Just (allforms y))) ++
    ((DisjunctionForm im) <$> [Nothing] <*> (map Just (allforms y))) ++ 
    ((DisjunctionForm im) <$> (map Just (allforms x)) <*> [Nothing]) ++  
    ((DisjunctionForm im) <$> [Nothing] <*> [Nothing] ) ++ 
    ((DisjunctionForm Nothing) <$> (map Just (allforms x)) <*> (map Just (allforms y))) ++
    ((DisjunctionForm Nothing) <$> [Nothing] <*> (map Just (allforms y))) ++ 
    ((DisjunctionForm Nothing) <$> (map Just (allforms x)) <*> [Nothing]) ++  
    ((DisjunctionForm Nothing) <$> [Nothing] <*> [Nothing] )
allforms (IndexedConditional im x y) =  
    ((ConditionalForm im) <$> (map Just (allforms x)) <*> (map Just (allforms y))) ++
    ((ConditionalForm im) <$> [Nothing] <*> (map Just (allforms y))) ++ 
    ((ConditionalForm im) <$> (map Just (allforms x)) <*> [Nothing]) ++  
    ((ConditionalForm im) <$> [Nothing] <*> [Nothing] ) ++ 
    ((ConditionalForm Nothing) <$> (map Just (allforms x)) <*> (map Just (allforms y))) ++
    ((ConditionalForm Nothing) <$> [Nothing] <*> (map Just (allforms y))) ++ 
    ((ConditionalForm Nothing) <$> (map Just (allforms x)) <*> [Nothing]) ++  
    ((ConditionalForm Nothing) <$> [Nothing] <*> [Nothing] )
allforms (IndexedBiconditional im x y) =  
    ((BiconditionalForm im) <$> (map Just (allforms x)) <*> (map Just (allforms y))) ++
    ((BiconditionalForm im) <$> [Nothing] <*> (map Just (allforms y))) ++ 
    ((BiconditionalForm im) <$> (map Just (allforms x)) <*> [Nothing]) ++  
    ((BiconditionalForm im) <$> [Nothing] <*> [Nothing] ) ++ 
    ((BiconditionalForm Nothing) <$> (map Just (allforms x)) <*> (map Just (allforms y))) ++
    ((BiconditionalForm Nothing) <$> [Nothing] <*> (map Just (allforms y))) ++ 
    ((BiconditionalForm Nothing) <$> (map Just (allforms x)) <*> [Nothing]) ++  
    ((BiconditionalForm Nothing) <$> [Nothing] <*> [Nothing] )

printform :: Form -> String 
printform (BasicForm mi) = case mi of 
   Just i -> show i
   Nothing -> "?" 
printform (NegationForm mi mp) = case mi of 
   Just i -> show i
   Nothing -> case mp of 
       Just p -> "~" ++ printform p
       Nothing -> "?"
printform (ConjunctionForm mi mx my) = case mi of 
   Just i -> show i 
   Nothing -> case mx of 
       Just x -> case my of 
             Just y -> "(" ++ printform x ++ "&" ++ printform y ++ ")" 
             Nothing -> "(" ++ printform x ++ "&?)"  
       Nothing -> case my of 
             Just y -> "(?&" ++ printform y ++ ")" 
             Nothing -> "?" 
printform (DisjunctionForm mi mx my) = case mi of 
   Just i -> show i 
   Nothing -> case mx of 
       Just x -> case my of 
             Just y -> "(" ++ printform x ++ "v" ++ printform y ++ ")" 
             Nothing -> "(" ++ printform x ++ "v?)"  
       Nothing -> case my of 
             Just y -> "(?v" ++ printform y ++ ")" 
             Nothing -> "?" 
printform (ConditionalForm mi mx my) = case mi of 
   Just i -> show i 
   Nothing -> case mx of 
       Just x -> case my of 
             Just y -> "(" ++ printform x ++ "->" ++ printform y ++ ")" 
             Nothing -> "(" ++ printform x ++ "->?)"  
       Nothing -> case my of 
             Just y -> "(?->" ++ printform y ++ ")" 
             Nothing -> "?" 
printform (BiconditionalForm mi mx my) = case mi of 
   Just i -> show i 
   Nothing -> case mx of 
       Just x -> case my of 
             Just y -> "(" ++ printform x ++ "<->" ++ printform y ++ ")" 
             Nothing -> "(" ++ printform x ++ "<->?)"  
       Nothing -> case my of 
             Just y -> "(<->&" ++ printform y ++ ")" 
             Nothing -> "?" 

normalise1 :: String -> String 
normalise1 x = let dups = nub (x \\ (nub x)) in 
    normalise dups x 

normalise :: String -> String -> String 
normalise _ [] = [] 
normalise y (x:xs) = if [x] `elem` (map show [0..100]) && x `elem` y then x : normalise y xs else 
    if [x] `elem` (map show [0..100]) 
       then '?' : normalise y xs 
       else x : normalise y xs 

normaliseindexing1 :: String -> String 
normaliseindexing1 x = normaliseindexing x x 

normaliseindexing :: String -> String -> String 
normaliseindexing _ [] = []
normaliseindexing y (x:xs) = if [x] `elem` (map show [0..100]) 
                                 then show(ints!(read [x])) ++ normaliseindexing y xs 
                                 else x : normaliseindexing y xs
    where ints = shuffle (nub (getints y))  

getints :: String -> [Int]
getints [] = []
getints (x:xs) = if [x] `elem` (map show [0..100]) 
                    then read [x] : getints xs 
                    else getints xs

shuffle :: [Int] -> Map.Map Int Int 
shuffle x = Map.fromList (zip x [0..])    


replaceconstant :: String -> String 
replaceconstant [] = []
replaceconstant (x:xs) = if Map.member [x] ilist then ilist![x] : replaceconstant xs else x : replaceconstant xs 

ilist :: Map.Map String Char
ilist = Map.fromList (zip (map show [0..100]) ['A'..])

replacevariable :: String -> String -> String 
replacevariable _ [] = []
replacevariable unused (x:xs) = if x == '?' then (head unused) : replacevariable (tail unused) xs else x : replacevariable unused xs 

tocanonical :: String -> String 
tocanonical x = let c = replaceconstant x in 
    let filtered = (filter (\x -> x `elem` ['A'..'Z']) c) in  
    let unused = if filtered == [] then ['A'..] else tail [maximum filtered ..] in 
    replacevariable unused c      

-- |Takes a proposition and returns a list of strings representing the forms it is an instance of
getforms :: Prop -> [String] 
getforms x = nub $ map (tocanonical . normaliseindexing1 . normalise1 . printform)  $ nub $ allforms $ indexprop x
