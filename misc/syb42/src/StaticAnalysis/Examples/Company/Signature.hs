module Examples.Company.Signature where

import Data.Set
import StrategyLib.Signature


companySig = Signature {
   sorts = fromList [
     "Company",
     "Departments",
     "Department",
     "Label",
     "Manager",
     "Units",
     "Unit",
     "Employee",
     "Name",
     "Salary"
   ], 
   symbols = fromList [
     ("Company",         ["Departments"],              "Company"),
     ("NilDepartments",  [],                           "Departments"),
     ("ConsDepartments", ["Department","Departments"], "Departments"),
     ("Department",      ["Label","Manager","Units"],  "Department"),
     ("Manager",         ["Employee"],                 "Manager"),
     ("NilUnits",        [],                           "Units"),
     ("ConsUnits",       ["Unit","Units"],             "Units"),
     ("EmployeeUnit",    ["Employee"],                 "Unit"),
     ("DepartmentUnit",  ["Department"],               "Unit"),
     ("Employee",        ["Name","Salary"],            "Employee")
   ]
 }

-- Tests

main :: IO Bool
main = do
  return (wellFormed companySig (fromList ["Label","Name","Salary"]))
