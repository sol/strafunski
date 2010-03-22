{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Examaples.Company.Reachability where

import StrategyLib.Baseline
import StrategyLib.Reachability
import Examples.Company.Types


-- Supply metadata for reachability

instance ReachableFrom [Department] Company
instance ReachableFrom Department Company
instance ReachableFrom Manager Company
instance ReachableFrom Employee Company
-- ...

{-

We should use more advanced type-class-based programming to take the
transitive closure of reachability programmatically. Otherwise, the
above specification, if it wanted to become complete, would be a
burden.

-}


-- Increase the salaries of all employees.

increase_all_salaries :: Company -> Company
increase_all_salaries = getId . full_td'' (Id . f)
 where
  f (Employee n s) = Employee n (s+1)
