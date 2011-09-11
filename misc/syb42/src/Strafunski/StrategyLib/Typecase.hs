{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module StrategyLib.Typecase where

import Data.Data
import StrategyLib.Baseline
import Data.HList.HListPrelude
import Data.HList.HTypeIndexed
import Data.HList.HOccurs
import Data.HList.TIP


-- Type-class-polymorphic type of familyT

class (Monad m, HTypeIndexed f) => FamilyT f m
  where
    familyT :: T m -> f -> T m


-- Empty list case

instance Monad m => FamilyT HNil m
  where 
    familyT g _ = g


-- Non-empty list case

instance ( Monad m
         , FamilyT t m
         , Data x
         , HOccursNot (x -> m x) t
         )
           => FamilyT (HCons (x -> m x) t) m
  where 
    familyT g (HCons h t) =  adhocT (familyT g t) h


-- Refined traversal schemes

full_td' s     = full_td (familyT idT s)
full_bu' s     = full_bu (familyT idT s)
once_td' s     = once_td (familyT failT s)
once_bu' s     = once_bu (familyT failT s)
stop_td' s     = stop_td (familyT failT s)
innermost' s   = innermost (familyT failT s)
