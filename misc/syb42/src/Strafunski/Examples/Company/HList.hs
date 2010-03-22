{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Examaples.Company.HList where

import Data.Tree
import Data.HList.HListPrelude hiding (Id)
import StrategyLib.Baseline
import StrategyLib.HList
import Examples.Company.Types


-- Increase the salaries of all employees.

increase_all_salaries :: Company -> Company
increase_all_salaries = getId . full_td' hlist
 where
  hlist = HCons (Id . f) HNil
  f (Employee n s) = Employee n (s+1)


-- Increase the salaries with distinction.

increase_all_salaries_with_distinction :: Company -> Company
increase_all_salaries_with_distinction = getId . full_td' hlist
 where
  hlist = HCons (Id . f) (HCons (Id . g) HNil)
  f (Employee n s) = Employee n (s+1)
  g (Manager (Employee n s)) = Manager (Employee n (s+1)) 
