module LogicTools.Filters.FilterPLProps (isValid, Filter) where

import LogicTools.Tables.Tables (validity)
import LogicTools.Data.PLProp 

type Filter = [Prop] -> Bool 

isValid :: Filter 
isValid = validity

