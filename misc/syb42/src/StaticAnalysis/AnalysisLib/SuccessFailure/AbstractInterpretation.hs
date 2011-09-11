-- Analysis of success/failure behavior
-- We use abstract interpretation (with little success).

import Prelude hiding (all,seq,repeat,Ord,(<=))
import SemanticsLib.Domain
import SemanticsLib.Fix
import StrategyLib.Syntax
import AnalysisLib.SuccessFailure.Sf


-- The actual analysis

analyse :: T Sf -> Sf
analyse Id            = ForallSuccess
analyse Fail          = ExistsFailure
analyse (Seq s s')    = analyse s `seq` analyse s'
analyse (Choice s s') = analyse s `choice` analyse s'
analyse (Var x)       = x
analyse (Rec f)       = fixEq (analyse . f)
analyse (All s)       = analyse s
analyse (One s)       = ExistsFailure


-- Tests

data Test = Full_bu | Full_td | Once_bu | Once_td | Stop_td | Innermost
 deriving (Show)

test :: Test -> IO ()
test t
 = test'
    (case t of
      Full_bu   -> full_bu
      Full_td   -> full_td
      Once_bu   -> once_bu
      Once_td   -> once_td
      Stop_td   -> stop_td
      Innermost -> innermost)
 where
  test' f = do
   test'' None
   test'' ForallSuccess
   test'' ExistsFailure
   test'' Any
   where
    test'' v = print $    show t
                       ++ " " 
                       ++ show v 
                       ++ " : " 
                       ++ show (analyse (f (Var v))) 

main = do
 test Full_bu
 test Full_td
 test Once_bu
 test Once_td
 test Stop_td
 test Innermost
