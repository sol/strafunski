module StrategyLib.Semantics where

import Prelude hiding (all)
import Data.Maybe
import SemanticsLib.Fix
import StrategyLib.Syntax


-- Terms

data Term   = Term Constr [Term]
type Constr = String


-- The semantic domain for strategies

type Meaning = Term -> Maybe Term


-- Compositional semantics

interpret :: TP Meaning -> Meaning
interpret Id            = Just
interpret Fail          = const Nothing
interpret (Seq s s')    = maybe Nothing (interpret s') . interpret s
interpret (Choice s s') = \t -> maybe (interpret s' t) Just (interpret s t)
interpret (Var x)       = x
interpret (Rec f)       = fixProperty (interpret . f)
interpret (All s)       = transform (all (interpret s))
interpret (One s)       = transform (one (interpret s))


-- Transform immediate subterms

transform :: ([Term] -> Maybe [Term]) -> Meaning
transform f (Term c ts)
 = maybe Nothing (Just . Term c) (f ts)


-- Transform all terms in a list
-- Note: this is a special instance of Control.Monad.sequence.

all :: Meaning -> [Term] -> Maybe [Term]
all f ts = kids ts'
 where
  ts' = map f ts
  kids [] = Just []
  kids (Just t':ts') = maybe Nothing (Just . (:) t') (kids ts')


-- Transform one term in a list

one :: Meaning -> [Term] -> Maybe [Term]
one f ts =  kids ts ts'
 where
  ts' = map f ts
  kids [] [] = Nothing
  kids (t:ts) (Nothing:ts') = maybe Nothing (Just . (:) t) (kids ts ts')
  kids (_:ts) (Just t':ts') = Just (t':ts)
