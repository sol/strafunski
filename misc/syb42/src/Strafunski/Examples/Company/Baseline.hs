module Examples.Company.Baseline where

import Data.Monoid
import Control.Monad
import StrategyLib.Baseline
import Examples.Company.Types


-- Increase the salaries of all employees.

increase_all_salaries :: TP Maybe
increase_all_salaries = full_td (idTP `adhocTP` f)
 where
  f (Employee n s) = Just (Employee n (s+1))

total_all_salaries :: TU Float
total_all_salaries = getSum . full_cl (adhocTU (constTU mempty) f)
 where
  f (Employee _ s) = Sum s


-- Total the salaries of all employees who are not managers.

total_all_non_managers :: TU Float
total_all_non_managers = getSum . stop_cl type_case
 where
  type_case :: TU (Maybe (Sum Float))
  type_case = constTU Nothing
              `adhocTU` employee
              `adhocTU` manager
  employee (Employee _ s) = Just (Sum s)
  manager (Manager _) = Just (Sum 0)
