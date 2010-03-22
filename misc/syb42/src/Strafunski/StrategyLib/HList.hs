{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module StrategyLib.HList where

import Data.Data
import StrategyLib.Baseline
import Data.HList.HListPrelude
import Data.HList.HTypeIndexed
import Data.HList.HOccurs
import Data.HList.TIP


-- Type-class-polymorphic type of familyTP

class (Monad m, HTypeIndexed f) => FamilyTP f m
  where
    familyTP :: TP m -> f -> TP m


-- Empty list case

instance Monad m => FamilyTP HNil m
  where 
    familyTP g _ = g


-- Non-empty list case

instance ( Monad m
         , FamilyTP t m
         , Data x
         , HOccursNot (x -> m x) t
         )
           => FamilyTP (HCons (x -> m x) t) m
  where 
    familyTP g (HCons h t) =  adhocTP (familyTP g t) h


-- Refined traversal schemes

full_td' s     = full_td (familyTP idTP s)
full_bu' s     = full_bu (familyTP idTP s)
once_td' s     = once_td (familyTP failTP s)
once_bu' s     = once_bu (familyTP failTP s)
stop_td' s     = stop_td (familyTP failTP s)
innermost' s   = innermost (familyTP failTP s)
