
{- |
Module : Random PL Prop
Copyright: (c) Ryan Cox, 2023
License: GPL-3

This module contains functions for generating random PL propositions

 -}


module LogicTools.Random.RandomPLProp 
   (randomPLprops
   , randomPLpropS
   , defaultSettings
   , getStdGen
   , Settings (..)
   , printRandomPLpropS
   , printFilteredRandomPLpropS )   where

import LogicTools.Data.PLProp 
import LogicTools.Filters.FilterPLProps
import Data.List
import System.Random


data Settings = Settings {minConstr :: Int -- minimum connectives in props
                         ,maxConstr :: Int -- maximum connectives in props
                         ,numProps :: Int -- how many propositions at a time
                         ,includeCons :: [Constructor] -- include connectives (possibly)
                         ,excludeCons :: [Constructor] -- exclude connectives (certainly)
                         ,basics :: String -- basics
                         }

defaultSettings :: Settings
defaultSettings = Settings {minConstr = 2
                     ,maxConstr = 3
                     ,numProps = 3
                     ,includeCons = [NegConstr Negation
                                  , CondConstr Conditional
                                  , ConjConstr Conjunction
                                  , DisjConstr Disjunction
                                  , BiconConstr Biconditional]
                     ,excludeCons = []
                     ,basics = "ABC"
                     }

data Constructor = NegConstr (Prop -> Prop)
                 | ConjConstr (Prop -> Prop -> Prop)
                 | DisjConstr (Prop -> Prop -> Prop)
                 | CondConstr (Prop -> Prop -> Prop)
                 | BiconConstr (Prop -> Prop -> Prop)

instance Eq Constructor where
    NegConstr _ == NegConstr _ = True
    ConjConstr _ == ConjConstr _ = True
    DisjConstr _ == DisjConstr _ = True
    CondConstr _ == CondConstr _ = True
    BiconConstr _ == BiconConstr _ = True
    _ == _ = False

-- | a stream of lists of random propositions
printRandomPLpropS :: IO ()
printRandomPLpropS = do 
   gen <- getStdGen
   mapM_ print $ map (map $ printprop) $ randomPLpropS gen defaultSettings 

-- | a stream of filtered lists of random propositions
printFilteredRandomPLpropS :: Filter -> IO ()
printFilteredRandomPLpropS f = do 
   gen <- getStdGen
   mapM_ print $ map (map $ printprop) $ filter f $ randomPLpropS gen defaultSettings 



-- | from a randomgen and settings return a list of propositions 
randomPLprops :: RandomGen g => g -> Settings -> [Prop]
randomPLprops gen s = head $ take 1 $ nrprops gen s

-- | from a randomgen and settings return an infinite
-- list of lists of propositions
randomPLpropS :: RandomGen g => g -> Settings -> [[Prop]]
randomPLpropS gen s = nrprops gen s



-- functions to generate random lists of lists of propositions, depending on
-- settings for basics to choose from, the number of connectives to draw from,
-- the list of connectives to draw from, and the number of propositions to include
-- in each list

r :: RandomGen g => g -> [a] -> a
r g x = x!!fst(randomR(0,length x -1) g)

gens :: RandomGen g => g -> [g]
gens g = g : gens (snd $ split g)

rsplitAt :: RandomGen g => g -> [a] -> ([a],[a])
rsplitAt g xs = let y = (fst $ randomR (0,length xs) g) in
    splitAt y xs

-- | get a list of constructors by taking the difference of the set of constructors
-- | to include and the list of constructors to exclude
constructors :: Settings -> [Constructor]
constructors s = includeCons s \\ excludeCons s

-- | get an infinite list of random constructors to build the proposition out of
rconstructors :: RandomGen g => g -> Settings -> [Constructor]
rconstructors g s = rconstructor g s : rconstructors g1 s
    where g1 = snd $ next g
          rconstructor g s = r g (constructors s)

-- | construct a proposition for a list of constructors
construct :: RandomGen g => g -> Settings -> [Constructor] -> Prop
construct h s (x:y:xs) = case x of NegConstr f -> f (construct (g!!1) s (y:xs))
                                   ConjConstr f -> constructlr (g!!2) s f (y:xs)
                                   DisjConstr f -> constructlr (g!!3) s f (y:xs)
                                   CondConstr f -> constructlr (g!!4) s f (y:xs)
                                   BiconConstr f -> constructlr (g!!5) s f (y:xs)
    where g = gens h
construct h s (x:xs) = case x of NegConstr f -> f (construct (g!!6) s xs)
                                 ConjConstr f -> constructlr (g!!7) s f xs
                                 DisjConstr f -> constructlr (g!!8) s f xs
                                 CondConstr f -> constructlr (g!!9) s f xs
                                 BiconConstr f -> constructlr (g!!10) s f xs
    where g = gens h
construct h s [] = rbasic (g!!11) s
    where g = gens h
          rbasic g s = Basic (r g (basics s))

-- | a helper function 
constructlr :: RandomGen g => g -> Settings -> (Prop -> Prop -> Prop) -> [Constructor] -> Prop
constructlr g s f xs = let (l,r) = rsplitAt g xs in
                       f (construct g1 s l) (construct g2 s r)
                       where (g1,g2) = split g

-- | generate an infinite random list of propositions given some setting
rprops' :: RandomGen g => g -> Settings -> [Prop]
rprops' g s = construct g1 s (take (r g4 [(minConstr s)..(maxConstr s)]) $ rconstructors g2 s) : rprops' g3 s
    where (g1,g2) = split g
          (g3,g4) = split g2

-- | generate an infinite random list of lists of propositions given some setting
nrprops :: RandomGen g => g -> Settings -> [[Prop]]
nrprops g s = chop (numProps s) (rprops' g s)
    where chop m xs = take m xs : chop m (drop m xs)
