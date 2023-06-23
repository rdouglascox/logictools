module LogicTools.Enum.EnumPL where 

import LogicTools.Data.PLProp 

tobasics :: [Char] -> [Prop]
tobasics x = fmap Basic x

tonegations :: [Prop] -> [Prop]
tonegations x = fmap Negation x

andnegations :: [Prop] -> [Prop] 
andnegations x = x ++ tonegations x

totwoplaces :: [Prop] -> [Prop] -> [Prop] 
totwoplaces x y = [Conjunction, Disjunction, Conditional, Biconditional] <*> x <*> y

expand :: ([Prop],[Prop],[Prop]) -> ([Prop],[Prop],[Prop])
expand (acc,old,new) = let newnew = andnegations $ (totwoplaces old new) ++ (totwoplaces new old) ++ (totwoplaces new new) in 
                       let newold = new in 
                       let newacc = acc ++ newnew in
    (newacc,newold,newnew)

getacc :: ([Prop],[Prop],[Prop]) -> [Prop]
getacc (x,_,_) = x 

applyNtimes :: Int -> (a -> a) -> a -> a
applyNtimes 1 f x = f x
applyNtimes n f x = f (applyNtimes (n-1) f x)

-- | Enumerate propositions based on basics [Char] with Int
-- expansions of the basic list. It is worth noting that
-- `length (enum 3 "A")` = 684,205,090. That's 684 million
-- propositions. In contrast, `length (enum 2 "ABCDEF")` is
-- only a modest 10,839,180 or 10 million propositions.   
enum :: Int -> [Char] -> [Prop]
enum n xs = getacc $ applyNtimes n expand (andnegations (tobasics xs),[], andnegations (tobasics xs))  
