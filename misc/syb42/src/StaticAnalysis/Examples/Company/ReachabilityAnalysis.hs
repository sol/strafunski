import Data.Set as Set
import StrategyLib.Syntax
import SemanticsLib.Map as Map
import AnalysisLib.Reachability
import Examples.Company.Signature hiding (main)

-- Tests

main = do
 let a = analyse companySig
 let incSalary = Map.fromList [("Salary",Set.fromList ["incSalary"])]
 print $ Map.lookup "Company" $ a Id
 print $ Map.lookup "Salary" $ a (Var incSalary)
 print $ Map.lookup "Employee" $ a (Var incSalary)
 print $ Map.lookup "Employee" $ a (All (Var incSalary))
 print $ Map.lookup "Department" $ a (All (Var incSalary))
 print $ Map.lookup "Department" $ a (once_bu (Var incSalary))
 print $ a (All (All (Var incSalary)))

main2 = do
 let a = analyse companySig
 let incSalary = Map.fromList [("Salary",Set.fromList ["incSalary"])]
 print $ a Id
 print $ a (Var incSalary)
 print $ a (All (Var incSalary))
 print $ a (All (All (Var incSalary)))
 print $ a (once_bu (Var incSalary))
 print $ a (once_td (Var incSalary))
 print $ a (once_td (Var incSalary)) == a (once_bu (Var incSalary))
 print $ a (stop_td (Var incSalary))

main3 = do
 let a = analyse companySig
 let section = Map.fromList [("Department",Set.fromList ["Sections"]),("Departments",Set.fromList ["Sections"])]--,
                            -- ("ConsDepartments",Set.fromList ["Sections"]), ("NilDepartments",Set.fromList ["Sections"])]
 print $ a Id
 print $ a (Var section)
 print $ a (All (Var section))
 print $ a (All (All (Var section)))
 print $ a (once_bu (Var section))
 print $ a (once_td (Var section))
 print $ a (stop_td (Var section))
 print $ a (full_td (Var section))
