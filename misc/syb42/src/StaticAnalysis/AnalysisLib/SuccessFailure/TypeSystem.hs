-- Type inference for success/failure behavior

module AnalysisLib.SuccessFailure.TypeSystem where

import Control.Monad
import StrategyLib.Syntax


-- Type expressions
type Type = Bool -- Can we conclude that there is definitely no failure?

-- Type inference
typeOf :: TP Type -> Maybe Type
typeOf Id             = Just True
typeOf Fail           = Just False
typeOf (Seq s s')     = liftM2 (&&) (typeOf s) (typeOf s')
typeOf (Choice s s')  = liftM2 (||) (typeOf s) (typeOf s')
typeOf (Var x)        = Just x
typeOf (Rec f)        = rec f True `mplus` rec f False
typeOf (All s)        = typeOf s
typeOf (One s)        = typeOf s >> Just False

-- Infer type of recursive closure by exhaustion
rec :: (Type -> TP Type) -> Type -> Maybe Type
rec f t = typeOf (f t) >>= \t' ->
            if t==t' then Just t else Nothing


-- Tests

data Test = Full_bu | Full_td | Once_bu | Once_td | Stop_bu | Stop_td | Innermost
 deriving (Show)

test :: Test -> IO ()
test t
 = test'
    (case t of
      Full_bu   -> full_bu
      Full_td   -> full_td
      Once_bu   -> once_bu
      Once_td   -> once_td
      Stop_bu   -> stop_bu
      Stop_td   -> stop_td
      Innermost -> innermost)
 where
  test' f = do
   test'' False
   test'' True
   where
    test'' v = print $    show t
                       ++ " " 
                       ++ show v 
                       ++ " : " 
                       ++ show (typeOf (f (Var v))) 

main = do
 test Full_bu
 test Full_td
 test Once_bu
 test Once_td
 test Stop_bu
 test Stop_td
 test Innermost
