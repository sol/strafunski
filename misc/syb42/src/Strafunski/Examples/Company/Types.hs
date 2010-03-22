{-# LANGUAGE DeriveDataTypeable #-}

module Examples.Company.Types where

import Data.Data

data  Company      = Company [Department]
 deriving (Data,Typeable,Show,Eq)
data  Department   = Department Name Manager [Unit]
 deriving (Data,Typeable,Show,Eq)
data  Manager      = Manager Employee
 deriving (Data,Typeable,Show,Eq)
data  Unit         = EmployeeUnit Employee
                   | DepartmentUnit Department
 deriving (Data,Typeable,Show,Eq)
data  Employee     = Employee Name Salary
 deriving (Data,Typeable,Show,Eq)
type  Name         = String   -- names of employees and departments
type  Salary       = Float    -- salaries of employees

myCompany = Company [dept1, dept2]
 where
  dept1 = Department "HR" lisa [EmployeeUnit ellen]
   where 
    lisa = Manager (Employee "Lisa" 12345)
    ellen = Employee "Ellen" 2345
  dept2 = Department "Dev" nerd [DepartmentUnit cs, DepartmentUnit vb]
   where
    nerd = Manager (Employee "Nerd" 11111)
    cs = Department "C#" anders [EmployeeUnit ada]
     where
      anders = Manager (Employee "Anders" 88888) 
      ada = Employee "Ada" 111
    vb = Department "VB" erik [EmployeeUnit miranda]
     where
      erik = Manager (Employee "Erik" 54321)
      miranda = Employee "Miranda" 1234
