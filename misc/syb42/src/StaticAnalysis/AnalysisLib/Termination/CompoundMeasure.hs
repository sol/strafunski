{-# LANGUAGE FlexibleInstances #-}

module AnalysisLib.Termination.CompoundMeasure where

import Prelude hiding (seq,(<=),(<),repeat)
import Data.Maybe
import SemanticsLib.Domain
import StrategyLib.Syntax
import AnalysisLib.Termination.Rel


-- Measures on terms
-- Note:
--  Constructor counts and term depth may be chained together.
--  Term depth always ends up in the least significant position by construction.
--  Other forms of composing measures are omitted here for simplicity's sake.

data Measure
 = Depth
 | Count Constr Measure

type Constr = String


-- Partial order and LUB for lists of Rels
-- Note: the head of the list is the most significant for the order.

instance POrd [Rel]
 where
  [] <= [] = True
  (r:rs) <= (r':rs') = if r==r' then rs<=rs' else r<=r'

instance Lub [Rel]
 where
  lub [] []  = []
  lub (r:rs) (r':rs') = lub r r' : lub rs rs'


-- Arithmetic for lists of Rels

pluslist :: [Rel] -> [Rel] -> [Rel]
pluslist [] [] = []
pluslist (r:rs) (r':rs') = plus r r' : lub rs rs'

increaseDepth [r] = [increase r]
increaseDepth (r:rs) = r : increaseDepth rs

decreaseDepth [r] = [decrease r]
decreaseDepth (r:rs) = r : decreaseDepth rs

increaselist :: [Rel] -> [Rel]
increaselist [r] = [increase r]
increaselist (r:rs)
 = let rs' = increaselist rs
   in if rs == rs'
        then if r == Any
          then r:rs
          else increase r : lesss' rs
        else r : rs'


-- Abstraction of term transformation

type Abs = [Rel] -> Maybe [Rel]


-- Distinguished lists of rels

lesss Depth = [Less]
lesss (Count _ m) = Less : lesss m

leqs Depth = [Leq]
leqs (Count _ m) = Leq : leqs m

anys Depth = [Any]
anys (Count _ m) = Any : anys m

leqs' [] = []
leqs' (_:rs) = Leq : leqs' rs

lesss' [] = []
lesss' (_:rs) = Less : lesss' rs

anys' [] = []
anys' (_:rs) = Any : anys' rs


-- Generate all values for a list of Rels

generate :: [Rel] -> [[Rel]]
generate rs = generate' (lesss' rs)
 where
  generate' rs = rs : if rs == rs' then [] else generate' rs'
   where rs' = increaselist rs


-- Entrance point for termination checking

typeOf :: T ([Rel],Bool) -> Measure -> Maybe [Rel]
typeOf s m = analysis s (leqs m)

-- Symbolic execution of strategies to compute relation on measure

analysis :: T ([Rel],Bool) -> Abs
analysis Id = Just
analysis Fail = Just
analysis (Seq s s') = maybe Nothing (analysis s') . analysis s

analysis (Choice s s')
 = \rs ->
     case (analysis s rs, analysis s' rs) of
       (Just rs1, Just rs2) -> Just $ lub rs1 rs2
       _                    -> Nothing

analysis (Var (rs,b))
 = \rs' ->
     if not b || rs' < leqs' rs
       then Just $ pluslist rs' rs
       else Nothing

analysis (Rec f)
 = \rs -> maybe Nothing (Just . pluslist rs) (typeOfClosure rs)
 where
  typeOfClosure rs = if null attempts
                       then Nothing 
                       else Just (head attempts)
   where
    attempts = catMaybes $ map wtClosure' (generate rs)
    wtClosure rs = maybe False (<=rs) (analysis (f (rs,True)) (leqs' rs))
    wtClosure' rs = if wtClosure rs then Just rs else Nothing

analysis (All s) = transform (analysis s)
analysis (One s) = transform (analysis s)


-- Measure effect for one-layer traversal 

transform :: Abs -> Abs
transform f rs
 =
   maybe
     Nothing
     (Just . increaseDepth)
     (f (decreaseDepth rs))

-- Tests

main = do

  -- Construct a variable from a delta
  let p d = Var (d,False)

  -- Some non-recursive cases
  putStrLn $ "nonrecursive1 :: " ++ show (typeOf (Rec (\_ -> p [Any])) Depth)
  putStrLn $ "nonrecursive2 :: " ++ show (typeOf (Rec (\_ -> p [Leq])) Depth)
  putStrLn $ "nonrecursive3 :: " ++ show (typeOf (Rec (\_ -> p [Less])) Depth)
  putStrLn $ "nonrecursive4 :: " ++ show (typeOf (Rec (\_ -> try (p [Less]))) Depth)

  -- Trivial forms of divergence (all False, hence)
  putStrLn $ "divergence1 :: " ++ show (typeOf (Rec (\v -> Var v)) Depth)
  putStrLn $ "divergence2 :: " ++ show (typeOf (Rec (\v -> Seq Id (Var v))) Depth) 

  -- Deep identity
  let deep_id = Rec (\v -> All (Var v))
  putStrLn $ "deep_id :: " ++ show (typeOf deep_id Depth)

  -- Traversal schemes
  putStrLn $ "full_bu Any :: " ++ show (typeOf (full_bu (p [Any])) Depth)
  putStrLn $ "full_td Any :: " ++ show (typeOf (full_td (p [Any])) Depth)
  putStrLn $ "full_td Leq :: " ++ show (typeOf (full_td (p [Leq])) Depth)
  putStrLn $ "full_td Less :: " ++ show (typeOf (full_td (p [Less])) Depth)
  putStrLn $ "stop_td Any :: " ++ show (typeOf (stop_td (p [Any])) Depth)
  putStrLn $ "once_bu Any :: " ++ show (typeOf (once_bu (p [Any])) Depth)
  putStrLn $ "once_bu Leq :: " ++ show (typeOf (once_bu (p [Leq])) Depth)
  putStrLn $ "once_bu Less :: " ++ show (typeOf (once_bu (p [Less])) Depth)
  let m = Count "foo" Depth
  putStrLn $ "full_bu [Less,Any] :: " ++ show (typeOf (full_bu (p [Less,Any])) m)
  putStrLn $ "full_td [Less,Any] :: " ++ show (typeOf (full_td (p [Less,Any])) m)
  putStrLn $ "once_bu [Less,Any] :: " ++ show (typeOf (once_bu (p [Less,Any])) m)

  -- Repeat
  putStrLn $ "repeat Any :: " ++ show (typeOf (repeat (p [Any])) Depth)
  putStrLn $ "repeat Leq :: " ++ show (typeOf (repeat (p [Leq])) Depth)
  putStrLn $ "repeat Less :: " ++ show (typeOf (repeat (p [Less])) Depth)
  putStrLn $ "repeat [Less,Any] :: " ++ show (typeOf (repeat (p [Less,Any])) m)

  -- Innermost (nested closures)
  putStrLn $ "innermost Any :: " ++ show (typeOf (innermost (p [Any])) Depth)
  putStrLn $ "innermost Leq :: " ++ show (typeOf (innermost (p [Leq])) Depth)
  putStrLn $ "innermost Less :: " ++ show (typeOf (innermost (p [Less])) Depth)
  putStrLn $ "innermost [Less,Any] :: " ++ show (typeOf (innermost (p [Less,Any])) m)
