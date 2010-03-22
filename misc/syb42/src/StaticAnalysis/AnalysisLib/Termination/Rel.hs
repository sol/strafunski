module AnalysisLib.Termination.Rel where

import SemanticsLib.Domain


-- Relation on measures

data Rel
 = Leq
 | Less
 | Any
 deriving (Eq,Show)


-- Partial order and LUB for Rel

instance POrd Rel
 where
  _    <= Any   = True
  Less <= Leq   = True  
  r    <= r'    = r == r'

instance Lub Rel
 where
  lub Less Leq  = Leq
  lub Leq  Less = Leq
  lub r    r'   = if r == r' then r else Any


-- Rel arithmetic

plus :: Rel -> Rel -> Rel
plus Less Less = Less
plus Less Leq  = Less
plus Leq  Less = Less
plus Leq  Leq  = Leq
plus _    _    = Any

decrease :: Rel -> Rel
decrease Leq  = Less
decrease Less = Less
decrease Any  = Any

increase :: Rel -> Rel
increase Less = Leq
increase Leq  = Any
increase Any  = Any
