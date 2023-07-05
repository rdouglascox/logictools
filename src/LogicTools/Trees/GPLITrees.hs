
{-|
Module      : GPLItrees
Description : Make tree proofs for PL
Copyright   : (c) Ryan Cox 2021
License     : BSD3
Maintainer  : ryan@rdouglascox.com
Stability   : experimental

The module exports the mktree function for generating tree proofs from PL.
-}

module LogicTools.Trees.GPLITrees
       (TreeRule (..)
     , PTree (..)
     , mktest
     , multiuni
     , maybetree
     , mktree
     , mktreeSafe
     , mktree'
     , printPTree
     , treeisoversized
     , treeisoversized1
     , allpathsclosed
     , allpathsclosed1
     , requirerules
     , exquirerules
     , lengthPaths
     , maxPath
     , minPath
     , minBranch
     , maxBranch
     , preptree
     , getapplyRule
     , getRule
     , getmodel
     , getmodels
     ) where

import qualified Data.Set as Set
import Data.Tree
import Data.List
import Data.Maybe

import LogicTools.Data.GPLIProp


data Model = Model {domain :: [Int]
                   ,referents :: [(Char, Int)]
                   ,extensions :: [(Char, [[Int]])]
                   }

-- |Data type for Proof Trees
data PTree = Leaf [AProp] | DeadLeaf [AProp] | Branch [AProp] (PTree, PTree)
    deriving (Eq,Show)

-- |Data type for annotated propositions. 
data AProp = AProp Prop Check Subs
    deriving (Eq,Show)

-- |Type synonyms for Checkmarks and Substitutions
type Check = Bool
type Subs = [Char]




-- currently getting this wrong: ((Gba->#xGxa)&(Gbb<->Gbb))

-- |MAIN TREE BUILDING FUNCTIONS

-- |Make a tree given a list of propositions. (May not terminate on some inputs)
mktree :: [Prop] -> PTree
mktree ps = mk (closure [] (preptree ps)) -- will use (preptree ps) to convert the input to the initial state
    where mk t = if finished t
                     then t
                     else mk (getapplyRule t)

mktreeSafe :: [Prop] -> Maybe PTree
mktreeSafe = maybetree 1000 




-- | Oversized or overgrown trees are considered failures

maybetree :: Int -> [Prop] -> Maybe PTree
maybetree n ps = let (t,r) = maybetree1 n (Just (preptree ps),[]) in
                     t

maybetree1 :: Int -> (Maybe PTree,[TreeRule]) -> (Maybe PTree,[TreeRule])
maybetree1 n (Just t,rs) | oversize n t = (Nothing,[])
                         | finished t = (Just $ closure [] t,rs)
                         | otherwise = maybetree1 n (getapplyRule' (Just t,rs))
maybetree1 n (Nothing,rs) = (Nothing,rs)


-- | Filter out oversized trees (by total number of propositions on the tree)
treeisoversized :: Int -> [Prop] -> Bool
treeisoversized n ps = let (t,r) = maybetree1 n (Just (preptree ps),[]) in
                       case t of Nothing -> False
                                 Just x  -> True

-- |Filter out oversized trees (by total number of proposition, single prop.
treeisoversized1 :: Int -> Prop -> Bool
treeisoversized1 n x = treeisoversized n [x]

-- | Require tree apply X rule

maybetree2 :: (Maybe PTree,[TreeRule]) -> (Maybe PTree,[TreeRule])
maybetree2 (Just t,rs) | finished t = ((Just $ closure [] t),rs)
                       | otherwise = maybetree2 (getapplyRule' (Just t,(rs)))


requirerules :: [TreeRule] -> [Prop] -> Bool
requirerules [] ps = True
requirerules rs ps = let (t,r) = maybetree2 (Just (preptree ps),[]) in
                     case t of Nothing -> False
                               Just x -> if and $ map (\x -> x `elem` r) rs
                                             then True
                                             else False


exquirerules :: [TreeRule] -> [Prop] -> Bool
exquirerules rs ps = let (t,r) = maybetree2 (Just (preptree ps),[]) in
                     case t of Nothing -> False
                               Just x -> if or $ map (\x -> x `elem` r) rs
                                             then False
                                             else True


-- | _Filter out oversized and non-tauts
treeisoversized' :: Int -> [Prop] -> Bool
treeisoversized' n ps = let (t,r) = maybetree1 n (Just (preptree ps),[]) in
                       case t of Nothing -> False
                                 Just x  -> if (allpathsclosed x) then True else False

allpathsclosed1 :: Int -> Prop -> Bool
allpathsclosed1 n x = treeisoversized' n [x]


oversize :: Int -> PTree -> Bool
oversize n t = (tsize t) > n

tsize :: PTree -> Int
tsize (Branch xs (l,r)) = (length xs) + (tsize l) + (tsize r)
tsize (Leaf xs) = length xs
tsize (DeadLeaf xs) = length xs


mktest t = if allpathsclosed t
               then putStrLn "\nall paths close!\n"
               else putStrLn "\nnot all paths close!\n"

-- mkptree :: String -> IO ()
-- mkptree x = putStrLn $ printPTree $ mktree $ [gpliParser x]

mktree' ps = do
             putStrLn (concatMap printProp ps)
             let tree = (preptree ps)
             mktree1' tree

-- |Is the tree finished?
finished :: PTree -> Bool
finished t = allpathsclosed t || overgrown t || isNothing (getRule t)

-- |Are all paths closed?
allpathsclosed :: PTree -> Bool
allpathsclosed (Branch _ (l,r)) = allpathsclosed l && allpathsclosed r
allpathsclosed (Leaf _) = False
allpathsclosed (DeadLeaf _) = True






mktree1' t = if finished t
                then do
                    putStrLn $ printPTree (closure [] t)
                    mktest (closure [] t)
                else do
                    let s = tsize t
                    if s < 100000
                        then do
                            putStrLn $ show $ s
                            putStrLn ("allpathsclosed?: " ++ (show $ allpathsclosed (getapplyRule t)))
                            putStrLn ("Unsaturated ID?: " ++ (show $ idunsat (getapplyRule t)))
                            putStrLn ("Unsaturated UNI?: " ++ (show $ uniunsat (getapplyRule t)))
                            putStrLn ("Next Rule?: " ++ (show $ getRule (getapplyRule t)))
                            putStrLn $ printPTree (getapplyRule t)
                            (mktree1' (getapplyRule t))
                        else do
                            putStrLn "Final\n"
                            putStrLn ("Subids?: " ++ (show $ idunsat (getapplyRule t)))
                            putStrLn $ printPTree (getapplyRule t)
                            mktest (getapplyRule t)

overgrown :: PTree -> Bool
overgrown t = if null $ ['a'..'t'] \\ (nub $ treeSubs t)
                  then True
                  else False








-- |GENERAL TREE BUILDING HELPER FUNCTIONS

-- |Add Branches
addb :: ([AProp],[AProp]) -> PTree -> PTree
addb ([],[]) x = x
addb (l,r) (Branch xs  (x,y)) = Branch xs  (addb (l,r) x, addb (l,r) y)
addb (l,r) (Leaf xs ) = Branch xs  (Leaf l , Leaf r )
addb (l,r) (DeadLeaf xs ) = DeadLeaf xs

-- |Add Non-Branching
addnb :: [AProp] -> PTree -> PTree
addnb [] x = x
addnb l (Branch xs  (x,y)) = Branch xs  (addnb l x, addnb l y)
addnb l (Leaf xs ) = Leaf (xs ++ l)
addnb l (DeadLeaf xs ) = DeadLeaf xs

-- |Data type for Tree Rules
data TreeRule = DoubleNegationRule
              | ConjunctionRule
              | NegatedDisjunctionRule
              | NegatedConditionalRule
              | NegatedExistentialRule
              | NegatedUniversalRule
              | DisjunctionRule
              | NegatedConjunctionRule
              | ConditionalRule
              | BiconditionalRule
              | NegatedBiconditionalRule
              | ExistentialRule
              | UniversalRule
              | SubstitutionRule
              deriving (Show,Eq,Ord)

-- |Function to get next tree rule
getRule :: PTree -> Maybe TreeRule
getRule t | null rules = Nothing
          | plrules rules = Just $ minimum rules
          | (minimum rules) == ExistentialRule = Just $ ExistentialRule
          | (minimum rules) == UniversalRule && uniunsat t = Just $ UniversalRule
          | null (rulessansuni rules) = Nothing
          | (minimum (rulessansuni rules)) == SubstitutionRule && idunsat t = Just $ SubstitutionRule
          | otherwise = Nothing
    where rules = listRules t

-- |Function to remove universal rules from the list of rules
rulessansuni :: [TreeRule] -> [TreeRule]
rulessansuni rs = filter (\x -> x /= UniversalRule) rs

-- |Are there no PL rules in the list of rules?
noplrules :: [TreeRule] -> Bool
noplrules rs = null $ filter isaplerule rs

-- |Are there PL rules in the list of rules?
plrules :: [TreeRule] -> Bool
plrules rs = not $ null $ filter isaplerule rs

isaplerule :: TreeRule -> Bool
isaplerule DoubleNegationRule = True
isaplerule ConjunctionRule = True
isaplerule NegatedDisjunctionRule = True
isaplerule NegatedConditionalRule  = True
isaplerule NegatedExistentialRule = True
isaplerule NegatedUniversalRule = True
isaplerule DisjunctionRule = True
isaplerule NegatedConjunctionRule = True
isaplerule ConditionalRule = True
isaplerule BiconditionalRule = True
isaplerule NegatedBiconditionalRule = True
isaplerule x = False

-- |Function to list all possible tree rules
listRules :: PTree -> [TreeRule]
listRules (Branch xs (l,r)) = extractRules xs ++ listRules l ++ listRules r
listRules (Leaf xs) = extractRules xs
listRules (DeadLeaf xs ) = []

-- |Function check if only exis and unis are in the list
uniexi :: [TreeRule] -> Bool
uniexi ts = and (map eitheruniexi ts)
    where eitheruniexi UniversalRule = True
          eitheruniexi ExistentialRule = True
          eitheruniexi _ = False

-- |Function to extract all rules from a list of annotated propositions

extractRules :: [AProp] -> [TreeRule]
extractRules  [] = []
extractRules  ((AProp ps False subs):xs) = case ps of
    (Conjunction _ _) -> ConjunctionRule : extractRules  xs
    (Disjunction _ _) -> DisjunctionRule : extractRules  xs
    (Conditional _ _) -> ConditionalRule : extractRules  xs
    (Biconditional _ _) -> BiconditionalRule : extractRules  xs
    (Universal _ _) -> UniversalRule : extractRules  xs
    (Existential _ _) -> ExistentialRule : extractRules  xs
    (Negation (Negation _)) -> DoubleNegationRule : extractRules  xs
    (Negation (Conjunction _ _)) -> NegatedConjunctionRule : extractRules  xs
    (Negation (Disjunction _ _)) -> NegatedDisjunctionRule : extractRules  xs
    (Negation (Conditional _ _)) -> NegatedConditionalRule : extractRules  xs
    (Negation (Biconditional _ _)) -> NegatedBiconditionalRule : extractRules  xs
    (Negation (Existential _ _)) -> NegatedExistentialRule : extractRules  xs
    (Negation (Universal _ _)) -> NegatedUniversalRule : extractRules  xs
    (Atomic (Predicate 'I') [_,_]) -> SubstitutionRule : extractRules  xs
    x -> extractRules  xs
extractRules  (x:xs) = extractRules  xs

-- |Function to apply a tree rule to a tree
applyRule :: TreeRule -> PTree -> PTree
applyRule DoubleNegationRule t = closure [] $ doublenegation t
applyRule ConjunctionRule t = closure [] $ conjunction t
applyRule NegatedDisjunctionRule t = closure [] $ negdisjunction t
applyRule NegatedConditionalRule t = closure [] $ negatedconditional t
applyRule NegatedExistentialRule t = closure [] $ negatedexistential t
applyRule NegatedUniversalRule t = closure [] $ negateduniversal t
applyRule DisjunctionRule t = closure [] $ disjunction t
applyRule NegatedConjunctionRule t = closure [] $ negatedconjunction t
applyRule ConditionalRule t = closure [] $ conditional t
applyRule BiconditionalRule t = closure [] $ biconditional t
applyRule NegatedBiconditionalRule t = closure [] $ negbiconditional t
applyRule UniversalRule t = closure [] $ universal t
applyRule ExistentialRule t = closure [] $ existential t
applyRule SubstitutionRule t = closure [] $ subids t

-- |Function to get and apply a tree rule

getapplyRule :: PTree -> PTree
getapplyRule t = case (getRule t) of Nothing -> t
                                     Just x -> (applyRule x) t

-- | Special version to keep track of applied rules
getapplyRule' :: (Maybe PTree,[TreeRule]) -> (Maybe PTree,[TreeRule])
getapplyRule' (Just t,rs) = case (getRule t) of Nothing -> (Just t,rs)
                                                Just x -> (Just (applyRule x t),x:rs)

-- |The Closure Rule
closure :: [AProp] -> PTree -> PTree
closure acc (Branch xs (y,z)) = Branch xs (closure (acc ++ xs) y, closure (acc ++ xs) z)
closure acc (Leaf xs) = if contra (acc ++ xs) || negid (acc ++ xs)
    then DeadLeaf xs
    else Leaf xs
closure acc (DeadLeaf xs) = DeadLeaf xs

contra :: [AProp] -> Bool
contra xs = let a = toPropsPos xs in
            let b = toPropsNeg xs in
              not $ Set.disjoint (Set.fromList a) (Set.fromList b)

negid :: [AProp] -> Bool
negid xs = negid1 $ toProps1 xs

negid1 :: [Prop] -> Bool
negid1 [] = False
negid1 ((Negation (Atomic (Predicate 'I') [x,y])):xs) = if x == y
                                                  then True
                                                  else negid1 xs
negid1 (x:xs) = negid1 xs

toProps1 :: [AProp] -> [Prop]
toProps1 [] = []
toProps1 ((AProp p _ _) :xs) = p : toProps1 xs

toProps :: [AProp] -> ([Prop],[Prop])
toProps [] = ([],[])
toProps ((AProp (Negation p) _ _) :xs) = (p : (fst $ toProps xs), (snd $ toProps xs))
toProps ((AProp p _ _) :xs) = ((fst $ toProps xs), p : (snd $ toProps xs))

toPropsPos :: [AProp] -> [Prop]
toPropsPos = map tPP

tPP :: AProp -> Prop
tPP (AProp p _ _ ) = p

toPropsNeg :: [AProp] -> [Prop]
toPropsNeg = concatMap tPN

tPN :: AProp -> [Prop]
tPN (AProp (Negation p) _ _ ) = [p]
tPN _ = []

-- |THE TREE RULES

-- |Conditional Rule (a model context-free branching rule)
conditional :: PTree -> PTree
conditional = conditional1 []

-- |Conditional Recursion
conditional1 :: [AProp] -> PTree -> PTree
conditional1 acc (Branch ps  (l,r)) =
    case ps of [] -> Branch (acc ++ ps)  (conditional1 [] l,conditional1 [] r) -- at the end of the list, reconstruct node, and recurse
               (AProp (Conditional x y) False subs:ys) -> -- if we match 
                   Branch (acc ++ (AProp (Conditional x y) True subs:ys)) (addb ([AProp (Negation x) False []],[AProp y False []]) l,addb ([AProp (Negation x) False []], [AProp y False []]) r)
               (z:zs) ->
                   conditional1 (acc ++ [z]) (Branch zs (l,r))
conditional1 acc (Leaf ps ) =
    case ps of [] -> Leaf (acc ++ ps)
               (AProp (Conditional x y) False subs:ys) ->
                   Branch (acc ++ (AProp (Conditional x y) True subs:ys))  (Leaf [(AProp (Negation x) False [])] ,Leaf [(AProp y False [])] ) -- the old AProp list at the leaf node should now be reconstructed at the branch,  
               (z:zs) ->
                   conditional1 (acc ++ [z]) (Leaf zs )
conditional1 acc (DeadLeaf ps ) = DeadLeaf ps

-- |BiBiconditional Rule (a model context-free branching rule)
biconditional :: PTree -> PTree
biconditional = biconditional1 []

-- |BiBiconditional Recursion
biconditional1 :: [AProp] -> PTree -> PTree
biconditional1 acc (Branch ps  (l,r)) =
    case ps of [] -> Branch (acc ++ ps)  (biconditional1 [] l,biconditional1 [] r) -- at the end of the list, reconstruct node, and recurse
               (AProp (Biconditional x y) False subs:ys) -> -- if we match 
                   Branch (acc ++ (AProp (Biconditional x y) True subs:ys)) (addb ([AProp x False [], AProp y False []],[AProp (Negation x) False [], AProp (Negation y) False []]) l,addb ([AProp x False [], AProp y False []], [AProp (Negation x) False [], AProp (Negation y) False []]) r)
               (z:zs) ->
                   biconditional1 (acc ++ [z]) (Branch zs (l,r))
biconditional1 acc (Leaf ps ) =
    case ps of [] -> Leaf (acc ++ ps)
               (AProp (Biconditional x y) False subs:ys) ->
                   Branch (acc ++ (AProp (Biconditional x y) True subs:ys))  (Leaf [(AProp x False []), AProp y False []] ,Leaf [(AProp (Negation x) False []), AProp (Negation y) False []] ) -- the old AProp list at the leaf node should now be reconstructed at the branch,  
               (z:zs) ->
                   biconditional1 (acc ++ [z]) (Leaf zs )
biconditional1 acc (DeadLeaf ps ) = DeadLeaf ps

-- |NegBiBiconditional Rule (a model context-free branching rule)
negbiconditional :: PTree -> PTree
negbiconditional = negbiconditional1 []

-- |NegBiBiconditional Recursion
negbiconditional1 :: [AProp] -> PTree -> PTree
negbiconditional1 acc (Branch ps  (l,r)) =
    case ps of [] -> Branch (acc ++ ps)  (negbiconditional1 [] l,negbiconditional1 [] r) -- at the end of the list, reconstruct node, and recurse
               (AProp (Negation (Biconditional x y)) False subs:ys) -> -- if we match 
                   Branch (acc ++ (AProp (Negation (Biconditional x y)) True subs:ys)) (addb ([AProp x False [], AProp (Negation y) False []],[AProp (Negation x) False [], AProp y False []]) l,addb ([AProp x False [], AProp (Negation y) False []], [AProp (Negation x) False [], AProp y False []]) r)
               (z:zs) ->
                   negbiconditional1 (acc ++ [z]) (Branch zs (l,r))
negbiconditional1 acc (Leaf ps ) =
    case ps of [] -> Leaf (acc ++ ps)
               (AProp (Negation (Biconditional x y)) False subs:ys) ->
                   Branch (acc ++ (AProp (Negation (Biconditional x y)) True subs:ys))  (Leaf [(AProp x False []), AProp (Negation y) False []] ,Leaf [(AProp (Negation x) False []), AProp y False []] ) -- the old AProp list at the leaf node should now be reconstructed at the branch,  
               (z:zs) ->
                   negbiconditional1 (acc ++ [z]) (Leaf zs )
negbiconditional1 acc (DeadLeaf ps ) = DeadLeaf ps


-- |Disjunction Rule (a model context-free branching rule)
disjunction :: PTree -> PTree
disjunction = disjunction1 []

-- |Disjunction Recursion
disjunction1 :: [AProp] -> PTree -> PTree
disjunction1 acc (Branch ps  (l,r)) =
    case ps of [] -> Branch (acc ++ ps)  (disjunction1 [] l,disjunction1 [] r) -- at the end of the list, reconstruct node, and recurse
               (AProp (Disjunction x y) False subs:ys) -> -- if we match 
                   Branch (acc ++ (AProp (Disjunction x y) True subs:ys)) (addb ([AProp x False []],[AProp y False []]) l,addb ([AProp x False []], [AProp y False []]) r)
               (z:zs) ->
                   disjunction1 (acc ++ [z]) (Branch zs (l,r))
disjunction1 acc (Leaf ps ) =
    case ps of [] -> Leaf (acc ++ ps)
               (AProp (Disjunction x y) False subs:ys) ->
                   Branch (acc ++ (AProp (Disjunction x y) True subs:ys))  (Leaf [(AProp x False [])] ,Leaf [(AProp y False [])] ) -- the old AProp list at the leaf node should now be reconstructed at the branch,  
               (z:zs) ->
                   disjunction1 (acc ++ [z]) (Leaf zs )
disjunction1 acc (DeadLeaf ps ) = DeadLeaf ps

-- |Disjunction Rule (rewritten)
disjunction' :: PTree -> PTree
disjunction' = disjunction1' []

-- |Disjunction Recursion
disjunction1' :: [AProp] -> PTree -> PTree
disjunction1' acc (Branch ps (l,r)) = disjunction2 acc ps (l,r)
disjunction1' acc (Leaf ps ) = disjunction3 acc ps
disjunction1' acc (DeadLeaf ps ) = DeadLeaf ps

disjunction2 :: [AProp] -> [AProp] -> (PTree,PTree) -> PTree
disjunction2 acc [] (l,r) = Branch acc (disjunction1' [] l,disjunction1' [] r)
disjunction2 acc (AProp (Disjunction x y) False subs:ys) (l,r) = Branch (acc ++ (AProp (Disjunction x y) True subs:ys)) (addb ([AProp x False []],[AProp y False []]) l,addb ([AProp x False []], [AProp y False []]) r)
disjunction2 acc (z:zs) (l,r) = disjunction1' (acc ++ [z]) (Branch zs (l,r))

disjunction3 :: [AProp] -> [AProp] -> PTree
disjunction3 acc [] = Leaf acc
disjunction3 acc (AProp (Disjunction x y) False subs:ys) = Branch (acc ++ (AProp (Disjunction x y) True subs:ys))  (Leaf [(AProp x False [])] ,Leaf [(AProp y False [])] )
disjunction3 acc (z:zs) = disjunction1' (acc ++ [z]) (Leaf zs )


-- |Negated Conjunction Rule
negatedconjunction :: PTree -> PTree
negatedconjunction = negatedconjunction1 []

-- |Negated Conjunction Recursion
negatedconjunction1 :: [AProp] -> PTree -> PTree
negatedconjunction1 acc (Branch ps  (l,r)) =
    case ps of [] -> Branch (acc ++ ps)  (negatedconjunction1 [] l,negatedconjunction1 [] r) -- at the end of the list, reconstruct node, and recurse
               (AProp (Negation (Conjunction x y)) False subs:ys) -> -- if we match 
                   Branch (acc ++ (AProp (Negation (Conjunction x y)) True subs:ys)) (addb ([AProp (Negation x) False []],[AProp (Negation y) False []]) l,addb ([AProp (Negation x) False []], [AProp (Negation y) False []]) r)
               (z:zs) ->
                   negatedconjunction1 (acc ++ [z]) (Branch zs (l,r))
negatedconjunction1 acc (Leaf ps ) =
    case ps of [] -> Leaf (acc ++ ps)
               (AProp (Negation (Conjunction x y)) False subs:ys) ->
                   Branch (acc ++ (AProp (Negation (Conjunction x y)) True subs:ys))  (Leaf [(AProp (Negation x) False [])] ,Leaf [(AProp (Negation y) False [])] ) -- the old AProp list at the leaf node should now be reconstructed at the branch,  
               (z:zs) ->
                   negatedconjunction1 (acc ++ [z]) (Leaf zs )
negatedconjunction1 acc (DeadLeaf ps ) = DeadLeaf ps

-- |Conjunction Rule (a model context-free non-branching rule)
conjunction :: PTree -> PTree
conjunction = conjunction1 []

-- |Conjunction Recursion
conjunction1 :: [AProp] -> PTree -> PTree
conjunction1 acc (Branch ps  (l,r)) =
    case ps of [] -> Branch (acc ++ ps)  (conjunction1 [] l,conjunction1 [] r)
               (AProp (Conjunction x y) False subs:ys) ->
                   Branch (acc ++ (AProp (Conjunction x y) True subs:ys))  (addnb [AProp x False [],AProp y False []] l,addnb [AProp x False [],AProp y False []] r)
               (z:zs) -> conjunction1 (acc ++ [z]) (Branch zs  (l,r))
conjunction1 acc (Leaf ps ) =
    case ps of [] -> Leaf (acc ++ ps)
               (AProp (Conjunction x y) False subs:ys) ->
                   Leaf (acc ++ (AProp (Conjunction x y) True subs:ys) ++ [AProp x False [] ,AProp y False []])
               (z:zs) -> conjunction1 (acc ++ [z]) (Leaf zs )
conjunction1 acc (DeadLeaf ps ) = DeadLeaf ps

-- |Negated Disjunction Rule (a model context-free non-branching rule)
negdisjunction :: PTree -> PTree
negdisjunction = negdisjunction1 []

-- |Negated Disjunction Recursion
negdisjunction1 :: [AProp] -> PTree -> PTree
negdisjunction1 acc (Branch ps  (l,r)) =
    case ps of [] -> Branch (acc ++ ps)  (negdisjunction1 [] l,negdisjunction1 [] r)
               (AProp (Negation (Disjunction x y)) False subs:ys) ->
                   Branch (acc ++ (AProp (Negation (Disjunction x y)) True subs:ys))  (addnb [AProp (Negation x) False [],AProp (Negation y) False []] l,addnb [AProp (Negation x) False [],AProp (Negation y) False []] r)
               (z:zs) -> negdisjunction1 (acc ++ [z]) (Branch zs  (l,r))
negdisjunction1 acc (Leaf ps ) =
    case ps of [] -> Leaf (acc ++ ps)
               (AProp (Negation (Disjunction x y)) False subs:ys) ->
                   Leaf (acc ++ (AProp (Negation (Disjunction x y)) True subs:ys) ++ [AProp (Negation x) False [] ,AProp (Negation y) False []])
               (z:zs) -> negdisjunction1 (acc ++ [z]) (Leaf zs )
negdisjunction1 acc (DeadLeaf ps ) = DeadLeaf ps

-- |Negated Conditional 
negatedconditional :: PTree -> PTree
negatedconditional = negatedconditional1 []

-- |Negated Conditional Recursion
negatedconditional1 :: [AProp] -> PTree -> PTree
negatedconditional1 acc (Branch ps  (l,r)) =
    case ps of [] -> Branch (acc ++ ps)  (negatedconditional1 [] l,negatedconditional1 [] r)
               (AProp (Negation (Conditional x y)) False subs:ys) ->
                   Branch (acc ++ (AProp (Negation (Conditional x y)) True subs:ys))  (addnb [AProp x False [],AProp (Negation y) False []] l,addnb [AProp x False [],AProp (Negation y) False []] r)
               (z:zs) -> negatedconditional1 (acc ++ [z]) (Branch zs  (l,r))
negatedconditional1 acc (Leaf ps ) =
    case ps of [] -> Leaf (acc ++ ps)
               (AProp (Negation (Conditional x y)) False subs:ys) ->
                   Leaf (acc ++ (AProp (Negation (Conditional x y)) True subs:ys) ++ [AProp x False [] ,AProp (Negation y) False []])
               (z:zs) -> negatedconditional1 (acc ++ [z]) (Leaf zs )
negatedconditional1 acc (DeadLeaf ps ) = DeadLeaf ps

-- |DoubleNegation Rule (a model context-free non-branching rule)
doublenegation :: PTree -> PTree
doublenegation = doublenegation1 []

doublenegation1 :: [AProp] -> PTree -> PTree
doublenegation1 acc (Branch ps  (l,r)) =
    case ps of [] ->
                   Branch (acc ++ ps)  (doublenegation1 [] l,doublenegation1 [] r)
               (AProp (Negation (Negation x)) False subs:ys) ->
                   Branch (acc ++ (AProp (Negation (Negation x)) True subs:ys))  (addnb [AProp x False []] l,addnb [AProp x False []] r)
               (z:zs) ->
                   doublenegation1 (acc ++ [z]) (Branch zs (l,r))
doublenegation1 acc (Leaf ps ) =
    case ps of [] ->
                   Leaf (acc ++ ps)
               (AProp (Negation (Negation  x)) False subs:ys) ->
                   Leaf (acc ++ (AProp (Negation (Negation x)) True subs:ys) ++ [AProp x False []])
               (z:zs) ->
                   doublenegation1 (acc ++ [z]) (Leaf zs )
doublenegation1 acc (DeadLeaf ps ) = DeadLeaf ps

-- |Negated Existential
negatedexistential :: PTree -> PTree
negatedexistential = negatedexistential1 []

negatedexistential1 :: [AProp] -> PTree -> PTree
negatedexistential1 acc (Branch ps  (l,r)) =
    case ps of [] ->
                   Branch (acc ++ ps)  (negatedexistential1 [] l,negatedexistential1 [] r)
               (AProp (Negation (Existential x p)) False subs:ys) ->
                   Branch (acc ++ (AProp (Negation (Existential x p)) True subs:ys))  (addnb [AProp (Universal x (Negation p)) False []] l,addnb [AProp (Universal x (Negation p)) False []] r)
               (z:zs) ->
                   negatedexistential1 (acc ++ [z]) (Branch zs (l,r))
negatedexistential1 acc (Leaf ps ) =
    case ps of [] ->
                   Leaf (acc ++ ps)
               (AProp (Negation (Existential x p)) False subs:ys) ->
                   Leaf (acc ++ (AProp (Negation (Existential x p)) True subs:ys) ++ [AProp (Universal x (Negation p)) False []])
               (z:zs) ->
                   negatedexistential1 (acc ++ [z]) (Leaf zs )
negatedexistential1 acc (DeadLeaf ps ) = DeadLeaf ps

-- |Negated Universal
negateduniversal :: PTree -> PTree
negateduniversal = negateduniversal1 []

negateduniversal1 :: [AProp] -> PTree -> PTree
negateduniversal1 acc (Branch ps  (l,r)) =
    case ps of [] ->
                   Branch (acc ++ ps)  (negateduniversal1 [] l,negateduniversal1 [] r)
               (AProp (Negation (Universal x p)) False subs:ys) ->
                   Branch (acc ++ (AProp (Negation (Universal x p)) True subs:ys))  (addnb [AProp (Existential x (Negation p)) False []] l,addnb [AProp (Existential x (Negation p)) False []] r)
               (z:zs) ->
                   negateduniversal1 (acc ++ [z]) (Branch zs (l,r))
negateduniversal1 acc (Leaf ps ) =
    case ps of [] ->
                   Leaf (acc ++ ps)
               (AProp (Negation (Universal x p)) False subs:ys) ->
                   Leaf (acc ++ (AProp (Negation (Universal x p)) True subs:ys) ++ [AProp (Existential x (Negation p)) False []])
               (z:zs) ->
                   negateduniversal1 (acc ++ [z]) (Leaf zs )
negateduniversal1 acc (DeadLeaf ps ) = DeadLeaf ps


-- |Universal Rule (a model context-sensitive rule)
universal :: PTree -> PTree
universal = universal1 [] []

-- |Universal Recursion. A list of names on the path so far -> Annotated props to rebuild the node -> Input tree -> Output tree
universal1 :: [Char] -> [AProp] -> PTree -> PTree
universal1 accnames accprops tree@(Branch ps (l,r)) =
    case ps of (AProp (Universal x y) False subs:ys) ->
                   if (null ((nub (accnames ++ treeNames tree)) \\ subs)) && (not $ null subs) -- TRUE if sat
                       then universal1 (accnames ++ apropNames ps) (accprops ++ [AProp (Universal x y) False subs]) (Branch ys (l,r)) -- if saturated 
                       else Branch (accprops ++ (AProp (Universal x y) False ((getUniSub subs (accnames ++ treeNames tree)):subs):ys)) (addnb [AProp (uniSub subs (accnames ++ treeNames tree) x y) False []] l,addnb [AProp (uniSub subs (accnames ++ treeNames tree) x y) False []] r) -- if not sat   
               (z:zs) -> universal1 (accnames ++ apropNames ps) (accprops ++ [z]) (Branch zs (l,r))
               [] -> Branch (accprops) (universal1 (apropNames ps ++ accnames) [] l,universal1 (apropNames ps ++ accnames) [] r)
universal1 accnames accprops (Leaf ps) =
    case ps of (AProp (Universal x y) False subs:ys) ->
                   if (null ((nub (accnames ++ apropNames ps)) \\ subs)) && (not $ null subs) -- test for saturation 
                       then universal1 (accnames ++ apropNames ps) (accprops ++ [AProp (Universal x y) False subs]) (Leaf ys) -- if saturated 
                       else Leaf (accprops ++ (AProp (Universal x y) False ((getUniSub subs (apropNames ps ++ accnames )):subs):ys) ++ [AProp (uniSub subs (apropNames ps ++ accnames) x y) False []]) -- if not saturaed 
               (z:zs) -> universal1 (accnames ++ apropNames ps) (accprops ++ [z]) (Leaf zs )
               [] -> Leaf (accprops ++ ps)
universal1 accnames accprops (DeadLeaf ps ) = DeadLeaf ps

-- |Helper functions for Universal Rule

-- |Function to get the name to substitute in
getUniSub :: [Char] -> [Char] -> Char
getUniSub currentsubs namesonpath = if not $ null $ ((nub namesonpath) \\ currentsubs)
                                        then minimum ((nub namesonpath) \\ currentsubs)
                                        else minimum $ (['a'..'t'] \\ currentsubs)

uniSub :: [Char] -> [Char] -> Char -> Prop -> Prop
uniSub currentsubs namesonpath out = uniSub1 (getUniSub currentsubs namesonpath) out

uniSub1 :: Char -> Char -> Prop -> Prop
uniSub1 sub out (Atomic (Predicate x) terms) = Atomic (Predicate x) (termSub sub out terms)
uniSub1 sub out (Negation x) = Negation (uniSub1 sub out x)
uniSub1 sub out (Conjunction x y) = Conjunction (uniSub1 sub out x) (uniSub1 sub out y)
uniSub1 sub out (Disjunction x y) = Disjunction (uniSub1 sub out x) (uniSub1 sub out y)
uniSub1 sub out (Conditional x y) = Conditional (uniSub1 sub out x) (uniSub1 sub out y)
uniSub1 sub out (Biconditional x y) = Biconditional (uniSub1 sub out x) (uniSub1 sub out y)
uniSub1 sub out (Universal x y) = if x == sub
                                     then (Universal x y)
                                     else (Universal x (uniSub1 sub out y))
uniSub1 sub out (Existential x y) = if x == sub
                                       then (Existential x y)
                                       else (Existential x (uniSub1 sub out y))


-- |Function to substitue a name (sub) for a variable (out)
termSub :: Char -> Char -> [Term] -> [Term]
termSub sub out [] = []
termSub sub out (Variable x:xs) = if x == out then (Constant sub : termSub sub out xs) else Variable x : termSub sub out xs
termSub sub out (x:xs) = x : termSub sub out xs

-- |Function to extract names from a tree
treeNames :: PTree -> [Char]
treeNames (Branch xs (l,r)) = apropNames xs ++ treeNames l ++ treeNames r
treeNames (Leaf xs) = apropNames xs
treeNames (DeadLeaf xs) = apropNames xs

treeSubs :: PTree -> [Char]
treeSubs (Branch xs (l,r)) = apropSubs xs ++ treeSubs l ++ treeSubs r
treeSubs (Leaf xs) = apropSubs xs
treeSubs (DeadLeaf xs) = apropSubs xs

-- |Function to extract names from a list of AProps
apropNames :: [AProp] -> [Char]
apropNames ps = concatMap f ps
    where f (AProp p _ _) = propNames p

apropSubs :: [AProp] -> [Char]
apropSubs ps = concatMap f ps
    where f (AProp _ _ x) = x

-- |Function to extract names from a Prop
propNames :: Prop -> [Char]
propNames (Atomic (Predicate _) xs) = termsNames xs
propNames (Negation l) = propNames l
propNames (Conjunction l r) = propNames l ++ propNames r
propNames (Disjunction l r) = propNames l ++ propNames r
propNames (Conditional l r) = propNames l ++ propNames r
propNames (Biconditional l r) = propNames l ++ propNames r
propNames (Universal _ p) = propNames p
propNames (Existential _ p) = propNames p

-- |Function to extract names from a list of terms
termsNames :: [Term] -> [Char]
termsNames [] = []
termsNames (Constant x:xs) = x : termsNames xs
termsNames (x:xs) = termsNames xs

-- |Existential Rule

existential :: PTree -> PTree
existential = existential1 [] []

existential1 :: [Char] -> [AProp] -> PTree -> PTree
existential1 acc2 acc1 (Branch ps (l,r)) =
    case ps of [] -> Branch (acc1 ++ ps)  (existential1 (apropNames ps ++ acc2) [] l,existential1 (apropNames ps ++ acc2) [] r)
               (AProp (Existential x y) False subs:ys) ->
                   Branch (acc1 ++ (AProp (Existential x y) True ((getExiSub (apropNames ps ++ acc2 ++ treeNames r ++ treeNames l)):subs):ys))  (addnb [AProp (exiSub (apropNames ps ++ acc2 ++ treeNames l) x y) False []] l,addnb [AProp (exiSub (apropNames ps ++ acc2 ++ treeNames r) x y) False []] r)
               (z:zs) -> existential1 (apropNames ps ++ acc2) (acc1 ++ [z]) (Branch zs (l,r))
existential1 acc2 acc1 (Leaf ps) =
    case ps of [] -> Leaf (acc1 ++ ps)
               (AProp (Existential x y) False subs:ys) ->
                   Leaf (acc1 ++ (AProp (Existential x y) True ((getExiSub (apropNames ps ++ acc2 )):subs):ys) ++ [AProp (exiSub (apropNames ps ++ acc2) x y) False []])
               (z:zs) -> existential1 (apropNames ps ++ acc2) (acc1 ++ [z]) (Leaf zs )
existential1 acc2 acc1 (DeadLeaf ps ) = DeadLeaf ps

-- |Exi-helpers

getExiSub :: [Char] -> Char
getExiSub nop = let this = (nub $ ['a'..'t']) \\ nop in
                if null this then '*' else minimum this

exiSub :: [Char] -> Char -> Prop -> Prop
exiSub nop out = exiSub1 (getExiSub nop) out

exiSub1 :: Char -> Char -> Prop -> Prop
exiSub1 sub out (Atomic (Predicate x) terms) = Atomic (Predicate x) (termSub sub out terms)
exiSub1 sub out (Negation x) = Negation (exiSub1 sub out x)
exiSub1 sub out (Conjunction x y) = Conjunction (exiSub1 sub out x) (exiSub1 sub out y)
exiSub1 sub out (Disjunction x y) = Disjunction (exiSub1 sub out x) (exiSub1 sub out y)
exiSub1 sub out (Conditional x y) = Conditional (exiSub1 sub out x) (exiSub1 sub out y)
exiSub1 sub out (Biconditional x y) = Biconditional (exiSub1 sub out x) (exiSub1 sub out y)
exiSub1 sub out (Universal x y) = if x == sub
                                     then (Universal x y)
                                     else (Universal x (exiSub1 sub out y))
exiSub1 sub out (Existential x y) = if x == sub
                                       then (Existential x y)
                                       else (Existential x (exiSub1 sub out y))

-- |Saturation Check
uniunsat :: PTree -> Bool
uniunsat = uniunsat1 [] []

-- |Saturation is defined thus: if the subs list contains ever name on
-- the path. this means that a \\ b should be null if saturated.

-- |Saturation Check Recursion; is the tree uniunsaturated? yes -> True, no -> False
uniunsat1 :: [Char] -> [AProp] -> PTree -> Bool
uniunsat1 acc2 acc1 tree@(Branch ps (l,r)) =
    case ps of (AProp (Universal x y) False subs:ys) -> -- the call when a unisat proposition is found 
                   if (null ((nub (acc2 ++ treeNames tree)) \\ subs)) && (not $ null subs) -- test for saturation
                       then uniunsat1 (apropNames ps ++ acc2) (acc1 ++ [AProp (Universal x y) False subs]) (Branch ys (l,r)) -- if saturated  
                       else True -- if uniunsaturated
               (z:zs) -> uniunsat1 (apropNames ps ++ acc2) (acc1 ++ [z]) (Branch zs (l,r))
               [] -> (uniunsat1 (apropNames ps ++ acc2) [] l) || (uniunsat1 (apropNames ps ++ acc2) [] r)
uniunsat1 acc2 acc1 (Leaf ps) =
    case ps of (AProp (Universal x y) False subs:ys) -> -- when we match on a unisat
                   if (null ((nub (acc2 ++ apropNames ps)) \\ subs)) && (not $ null subs) -- test for saturation  
                       then uniunsat1 (apropNames ps ++ acc2) (acc1 ++ [AProp (Universal x y) False subs]) (Leaf ys) -- if saturated     
                       else True
               (z:zs) -> uniunsat1 (apropNames ps ++ acc2) (acc1 ++ [z]) (Leaf zs )
               [] -> False
uniunsat1 acc2 acc1 (DeadLeaf ps ) = False


-- |Substition of Identicals

-- |Apply substitution of identicals rule where possible.
subids :: PTree -> PTree
subids t = subids1 ([],[]) t

-- |Top Level Function, for calls onto new nodes (accumulates ps)
subids1 :: ([Prop],[AProp]) -> PTree -> PTree
subids1 (ps,ap) (Branch xs (l,r)) = subidsB ((ps ++ anaprops xs),ap) xs (l,r)
subids1 (ps,ap) (Leaf xs) = subidsL ((ps ++ anaprops xs),ap) xs
subids1 (ps,ap) (DeadLeaf xs) = DeadLeaf xs

-- |Top Level Function, for calls onto old nodes (does not accumulate ps)
subids2 :: ([Prop],[AProp]) -> PTree -> PTree
subids2 (ps,ap) (Branch xs (l,r)) = subidsB (ps,ap) xs (l,r)
subids2 (ps,ap) (Leaf xs) = subidsL (ps,ap) xs
subids2 (ps,ap) (DeadLeaf xs) = DeadLeaf xs

-- |Branching Case
subidsB :: ([Prop],[AProp]) -> [AProp] -> (PTree,PTree) -> PTree
subidsB (ps,ap) [] (l,r) = Branch ap (subids1 (ps,[]) l,subids1 (ps,[]) r)
subidsB (ps,ap) (AProp (Atomic (Predicate 'I') [Constant x,Constant y]) False _:xs) (l,r) =
    if anyPath x y ps l r
        then Branch (ap ++ (AProp (Atomic (Predicate 'I') [Constant x,Constant y]) False []:xs)) (subidhelper x y ps l,subidhelper x y ps r)
        else subids2 (ps,(ap ++ [(AProp (Atomic (Predicate 'I') [Constant x,Constant y]) False [])])) (Branch xs (l,r))
subidsB (ps,ap) (x:xs) (l,r) = subids2 (ps,(ap ++ [x])) (Branch xs (l,r))

-- |Non-Branching Case (correct in simple case)
subidsL :: ([Prop],[AProp]) -> [AProp] -> PTree
subidsL (ps,ap) [] = Leaf ap
subidsL (ps,ap) (AProp (Atomic (Predicate 'I') [Constant x,Constant y]) False _:xs) =
    if anyProp x y ps
        then Leaf (ap ++ (AProp (Atomic (Predicate 'I') [Constant x,Constant y]) False []:xs) ++ (nextAProp x y ps))
        else subids2 (ps,(ap ++ [(AProp (Atomic (Predicate 'I') [Constant x,Constant y]) False [])])) (Leaf xs)
subidsL (ps,ap) (x:xs) = subids2 (ps,(ap ++ [x])) (Leaf xs)

-- |Add nextAProp on Leaf nodes (will need to continue to accumulate
-- proposition on the relevant paths

subidhelper :: Char -> Char -> [Prop] -> PTree -> PTree
subidhelper x y acc (Branch xs (l,r)) = Branch xs ((subidhelper x y (acc ++ anaprops xs) l), (subidhelper x y (acc ++ anaprops xs) r))
subidhelper x y acc (Leaf xs) = Leaf (xs ++ (nextAProp x y (acc ++ anaprops xs)))
subidhelper x y acc (DeadLeaf xs) = DeadLeaf xs

-- |HELPERS FOR SUBIDS

-- |Get Atomics and Negations from a list of AProps (For accumulating props)
anaprops :: [AProp] -> [Prop]
anaprops [] = []
anaprops ((AProp (Atomic x y) _ _):xs) = (Atomic x y) : anaprops xs
anaprops ((AProp (Negation (Atomic x y)) _ _):xs) = (Negation (Atomic x y)) : anaprops xs
anaprops (x:xs) = anaprops xs

-- |Get Atomic or Negated Atomic or nothing from an AProp
anaprop :: AProp -> [Prop]
anaprop (AProp (Atomic x y) _ _) = [(Atomic x y)]
anaprop (AProp (Negation (Atomic x y)) _ _) = [(Negation (Atomic x y))]
anaprop x = []

-- |Any paths
anyPath :: Char -> Char -> [Prop] -> PTree -> PTree -> Bool
anyPath x y ps l r = or $ map (anyProp x y) (propsonpaths ps l r)


-- |Get lists of props on paths
propsonpaths :: [Prop] -> PTree -> PTree -> [[Prop]]
propsonpaths ps l r = map (ps ++) (pathhelper l ++ pathhelper r)

pathhelper :: PTree -> [[Prop]]
pathhelper t = pathhelper' [] t

pathhelper' :: [Prop] -> PTree -> [[Prop]]
pathhelper' acc (Branch xs (l, r)) = pathhelper' (acc ++ anaprops xs) l ++ pathhelper' (acc ++ anaprops xs) r
pathhelper' acc (Leaf xs) = [acc ++ (anaprops xs)]
pathhelper' acc (DeadLeaf xs) = []



-- |Get Atomics and Negations from a PTree

tprops :: PTree -> [Prop]
tprops t = tprops1 [] t

tprops1 :: [Prop] -> PTree -> [Prop]
tprops1 acc (Branch xs (l,r)) =  tprops1 (anaprops xs) l ++ tprops1 (anaprops xs) r
tprops1 acc (Leaf xs) = acc ++ (anaprops xs)
tprops1 acc (DeadLeaf xs) = []


-- |Takes each side of an identity and returns the next AProp (correct)
nextAProp :: Char -> Char -> [Prop] -> [AProp]
nextAProp i o ps = if null (nextProp i o ps)
                       then []
                       else [(AProp (head (nextProp i o ps)) False [])]

-- |Takes each side of an identity and returns the next Prop (correct)
nextProp :: Char -> Char -> [Prop] -> [Prop]
nextProp i o ps = (nub $ ((getProps i o ps) ++ (getProps o i ps))) \\  (nub ps)

-- |Test whether a new prop can be made from existing props and each side of an id  
anyProp :: Char -> Char -> [Prop] -> Bool
anyProp i o ps = not $ null $ nextProp i o ps

getProps :: Char -> Char -> [Prop] -> [Prop]
getProps i o ps = concatMap (getProps1 i o) ps

getProps1 :: Char -> Char ->  Prop -> [Prop]
getProps1 i o (Atomic x ts) = map (Atomic x) (getTerms i o ts)
getProps1 i o (Negation (Atomic x ts)) = map (\y -> (Negation (Atomic x y))) (getTerms i o ts)

-- | SubInt  Subout  TermsIn      TermsOut
getTerms :: Char -> Char -> [Term] -> [[Term]]
getTerms i o ts = sort $ map (s3 o ts) (s2 i (s5 o ts))

s1 :: String -> String -> [String]
s1 acc [] = [acc]
s1 acc xs = [acc] ++ s1 xs (tail xs)

s2 :: Char -> String -> [String]
s2 c xs = nub $ concatMap permutations $ map (\x -> (x ++ (replicate ((length xs) - (length x)) c))) (s1 [] xs)

-- | A function that takes a list of terms an in and out character,
-- and returns all possible substitutions

s3 :: Char -> [Term] -> String -> [Term]
s3 c (Constant t:ts) (x:xs) = if c == t
                                  then (Constant x: s3 c ts xs)
                                  else (Constant t: s3 c ts (x:xs))
s3 c (t:ts) (x:xs) = s3 c ts (x:xs)
s3 c ts [] = ts

s5 :: Char -> [Term] -> String
s5 c [] = []
s5 c (Constant t:ts) = if c == t
                           then c : s5 c ts
                           else s5 c ts
s5 c (t:ts) = s5 c ts

-- |END SUBIDS HELPERS




-- |Saturation - Identity Case
idunsat :: PTree -> Bool
idunsat t = idsats1 ([],[]) t


-- |Top Level Function, for calls onto new nodes (accumulates ps)
idsats1 :: ([Prop],[AProp]) -> PTree -> Bool
idsats1 (ps,ap) (Branch xs (l,r)) = idsatsB ((ps ++ anaprops xs),ap) xs (l,r)
idsats1 (ps,ap) (Leaf xs) = idsatsL ((ps++ anaprops xs),ap) xs
idsats1 (ps,ap) (DeadLeaf xs) = False

-- |Top Level Function, for calls onto old nodes (does not accumulate ps)
idsats2 :: ([Prop],[AProp]) -> PTree -> Bool
idsats2 (ps,ap) (Branch xs (l,r)) = idsatsB (ps,ap) xs (l,r)
idsats2 (ps,ap) (Leaf xs) = idsatsL (ps,ap) xs
idsats2 (ps,ap) (DeadLeaf xs) = False

-- |Branching Case
idsatsB :: ([Prop],[AProp]) -> [AProp] -> (PTree,PTree) -> Bool
idsatsB (ps,ap) [] (l,r) = (idsats1 (ps,[]) l) || (idsats1 (ps,[]) r)
idsatsB (ps,ap) (AProp (Atomic (Predicate 'I') [Constant x,Constant y]) False _:xs) (l,r) =
    if anyPath x y ps l r
        then True
        else idsats2 (ps,(ap ++ [(AProp (Atomic (Predicate 'I') [Constant x,Constant y]) False [])])) (Branch xs (l,r))
idsatsB (ps,ap) (x:xs) (l,r) = idsats2 (ps,(ap ++ [x])) (Branch xs (l,r))

-- |Non-Branching Case (correct in simple case)
idsatsL :: ([Prop],[AProp]) -> [AProp] -> Bool
idsatsL (ps,ap) [] = False
idsatsL (ps,ap) (AProp (Atomic (Predicate 'I') [Constant x,Constant y]) False _:xs) =
    if anyProp x y ps
        then True
        else idsats2 (ps,(ap ++ [(AProp (Atomic (Predicate 'I') [Constant x,Constant y]) False [])])) (Leaf xs)
idsatsL (ps,ap) (x:xs) = idsats2 (ps,(ap ++ [x])) (Leaf xs)

-- |Add nextAProp on Leaf nodes (will need to continue to accumulate
-- proposition on the relevant paths




-- |Function to prepare a tree from a list of propositions
preptree :: [Prop] -> PTree
preptree [] = Leaf []
preptree (x:xs) = Leaf (AProp x False []: prepprop xs)

-- |Function from Props to AProp for setting up the tree 
prepprop [] = []
prepprop (x:xs) = (AProp x False []: prepprop xs)


-- |Tree stat functions and filters

multiuni :: Int -> PTree -> Bool
multiuni n (Branch xs (l,r)) = mu n xs || multiuni n l || multiuni n r
multiuni n (Leaf xs) = mu n xs
multiuni n (DeadLeaf xs) = mu n xs

mu :: Int -> [AProp] -> Bool
mu n [] = False
mu n (AProp (Universal x p) _ subs : xs)  = if (length subs) >= n then True else mu n xs
mu n (x:xs) = mu n xs

-- |max path
maxPath :: Int -> PTree -> Bool
maxPath n t = (maximum $ lengthPaths t) <= n

-- |min path

minPath :: Int -> PTree -> Bool
minPath n t = (minimum $ lengthPaths t) >= n

-- |path lengths
lengthPaths :: PTree -> [Int]
lengthPaths = lengthPaths1 []

lengthPaths1 :: [Int] -> PTree -> [Int]
lengthPaths1 n (Branch xs (l,r)) = lengthPaths1 [length xs] l ++ lengthPaths1 [length xs] r
lengthPaths1 n (Leaf xs) = length xs : n
lengthPaths1 n (DeadLeaf xs) = length xs : n

-- |max branch

maxBranch :: Int -> PTree -> Bool
maxBranch n t = numBranch t <= n

-- |min branch

minBranch :: Int -> PTree -> Bool
minBranch n t = numBranch t >= n

-- | numBranch

numBranch :: PTree -> Int
numBranch (Branch xs (l,r)) = 1 + numBranch l + numBranch r
numBranch _ = 0


-- |Tests

a1 = Atomic (Predicate 'G') [Constant 'a',Constant 'b']
a2 = Atomic (Predicate 'H') [Constant 'b',Constant 'a']
i1 = Atomic (Predicate 'I') [Constant 'a',Constant 'b']
c1 = Conjunction a1 i1
n1 = Negation a1
c2 = Conjunction n1 i1

d1 = Disjunction a1 i1
d2 = Disjunction a1 a2

-- |READING A MODEL OFF A TREE

-- | A domain is a possibly empty list of objects
-- | An assigment of referents to names is a possibly empty list of
-- pairs of names and objects
-- | An assigment of extensions to predicates is a list of pairs of
-- predicates and either, lists of names, list of pairs of names, list
-- of triples of names, and so on.


type Path = [Prop]

getallopenpaths :: PTree -> [Path]
getallopenpaths = getallpaths1 []

getallpaths1 :: [Prop] -> PTree -> [[Prop]]
getallpaths1 acc (Branch xs (l, r)) = getallpaths1 (acc ++ aprop2prop xs) l ++ getallpaths1 (acc ++ aprop2prop xs) r
getallpaths1 acc (Leaf xs) = [acc ++ aprop2prop xs]
getallpaths1 acc (DeadLeaf xs) = []

aprop2prop :: [AProp] -> [Prop]
aprop2prop = map ap2p

ap2p :: AProp -> Prop
ap2p p = case p of { AProp pr b s -> pr }

getnamesonpath :: Path -> String
getnamesonpath p = sort $ nub $ concatMap getnames' p


-- | get names from any prop
getnames' :: Prop -> String
getnames' p = case p of
  Atomic pred tes -> case tes of
    [] -> []
    te : tes' -> case te of
      Variable c -> fromterms tes'
      Constant c -> c : fromterms tes'
  Negation pr -> getnames' pr
  Existential c pr -> getnames' pr
  Universal c pr -> getnames' pr
  Conjunction pr pr' -> getnames' pr ++ getnames' pr'
  Disjunction pr pr' -> getnames' pr ++ getnames' pr'
  Conditional pr pr' -> getnames' pr ++ getnames' pr'
  Biconditional pr pr' -> getnames' pr ++ getnames' pr'
  where fromterms [] = []
        fromterms (Variable c:xs) = fromterms xs
        fromterms (Constant c:xs) = c : fromterms xs

getpredsonpath :: Path -> String
getpredsonpath p = removeid $ sort $ nub $ concatMap getpreds p

getpreds :: Prop -> String
getpreds p = case p of
  Atomic (Predicate c) tes -> [c]
  Negation pr -> getpreds pr
  Existential c pr -> getpreds pr
  Universal c pr -> getpreds pr
  Conjunction pr pr' -> getpreds pr ++ getpreds pr'
  Disjunction pr pr' -> getpreds pr ++ getpreds pr'
  Conditional pr pr' -> getpreds pr ++ getpreds pr'
  Biconditional pr pr' -> getpreds pr ++ getpreds pr'

removeid :: String -> String
removeid = filter (/= 'I')

temprefs :: Path -> [(Char,Int)]
temprefs p = zip (getnamesonpath p) [1..]

getids :: Path -> [(Char,Char)]
getids p = concatMap getid p
    where getid (Atomic (Predicate 'I') (Constant x:Constant y:[])) = [(x,y)]
          getid _ = []

keyval :: (Eq a) => a -> [(a,b)] -> b
keyval x (y:ys) = if (fst y) == x
    then snd y
    else keyval x ys

changeval :: (Eq a) => a -> b -> [(a,b)] -> [(a,b)]
changeval x z (y:ys) = if (fst y) == x
    then (fst y,z) : changeval x z ys
    else y : changeval x z ys
changeval x z [] = []

trim :: [(Char,Int)] -> (Char,Char) -> [(Char,Int)]
trim r (fst,snd) = changeval snd (keyval fst r) r

finalrefs :: Path -> [(Char,Int)]
finalrefs p = foldl trim (temprefs p) (getids p)

makedomain :: [(Char,Int)] -> [Int]
makedomain xs = sort $ nub $ map dom xs
    where dom (fst,snd) = snd

namestorefs :: String -> [(Char,Int)] -> [Int]
namestorefs xs r = map (nametoref r) xs
    where nametoref r x = keyval x r

makepreds :: Path -> [(Char,[[Int]])]
makepreds p = nub $ concatMap pred p
    where pred (Atomic (Predicate 'I') y) = []
          pred (Negation (Atomic (Predicate 'I') y)) = []
          pred (Atomic (Predicate x) y) = [(x,[(namestorefs (f y) (finalrefs p))])]
          pred _ = []
          f (Constant x:xs) = x : (f xs)
          f [] = []

keyval' :: (Eq a) => a -> [(a,b)] -> [b]
keyval' x (y:ys) = if (fst y) == x
    then [snd y] ++ keyval' x ys
    else keyval' x ys
keyval' x [] = []

getall :: [(Char,[a])] -> Char -> (Char,[a])
getall xs x = (x,(concat (keyval' x xs)))

getall' :: [Char] -> [(Char,[a])] -> [(Char,[a])]
getall' ys xs = map (getall xs) ys

preds :: Path -> [(Char,[[Int]])]
preds p = nub $ getall' (getpredlet p) (makepreds p)

getpredlet :: Path -> [Char]
getpredlet p = nub $ getpredsonpath p


readmodel :: Path -> Model
readmodel p = Model (makedomain (finalrefs p)) (finalrefs p) (preds p)

readmodels :: PTree -> [Model]
readmodels t = map readmodel (getallopenpaths t)

getmodel :: PTree -> Model
getmodel t = head $ readmodels t

getmodels :: PTree -> [Model]
getmodels = readmodels


sp = putStrLn . printPTree

printPTree :: PTree -> String
printPTree = drawTree . ptree2Tree

ptree2Tree :: PTree -> Tree String
ptree2Tree (Branch xs (l,r)) = Node (printAProps xs) [ptree2Tree l, ptree2Tree r] 
ptree2Tree (Leaf xs) = Node ((printAProps xs) ++ " <- open") []
ptree2Tree (DeadLeaf xs) = Node ((printAProps xs) ++ " x") [] 

printAProps :: [AProp] -> String
printAProps [] = []
printAProps (x:xs) = printAProp x ++ ", " ++ printAProps xs 

printAProp :: AProp -> String
printAProp (AProp p True s) = printProp p ++ " checked" ++ " " ++ sort s 
printAProp (AProp p False s) = printProp p ++ " " ++ sort s 

printProp :: Prop -> String
printProp (Atomic (Predicate x) xs) = [x] ++ printTerms xs
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
