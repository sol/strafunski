{-# LANGUAGE FlexibleInstances #-} 

module AnalysisLib.Termination.DepthMeasure where

import Prelude hiding (seq,(<=),(<),repeat)
import Data.Maybe
import SemanticsLib.Domain
import StrategyLib.Syntax
import AnalysisLib.Termination.Rel


-- Distinction for Rel

type TRel = Rel  -- Term property
type SRel = Rel  -- Strategy property


-- Abstraction of term transformation

type Abs = TRel -> Maybe TRel


-- Entrance point for termination checking

typeOf :: T (SRel,Bool) -> Maybe SRel
typeOf s = analyse s Leq


-- Symbolic execution of strategies to compute relation on measure

analyse :: T (SRel,Bool) -> Abs

analyse Id = Just
analyse Fail = Just

analyse (Seq s s')
 = maybe Nothing (analyse s') . analyse s

analyse (Choice s s')
 = \r ->
     case (analyse s r, analyse s' r) of
       (Just r1, Just r2) -> Just $ lub r1 r2
       _                  -> Nothing

analyse (Var (r,b))
 = \r' ->
     if not b || r' < Leq
       then Just $ plus r' r
       else Nothing

analyse (Rec f)
 = \r -> maybe Nothing (Just . plus r) (typeOfClosure r)
 where
  typeOfClosure r = if null attempts
                      then Nothing 
                      else Just (head attempts)
   where
    attempts = catMaybes $ map wtClosure' [Less,Leq,Any]
    wtClosure r = maybe False (<=r) (analyse (f (r,True)) Leq)
    wtClosure' r = if wtClosure r then Just r else Nothing

analyse (All s) = transform (analyse s)
analyse (One s) = transform (analyse s)

transform :: Abs -> Abs
transform f r
 =
   maybe
     Nothing
     (Just . increase)
     (f (decrease r))


-- Tests

main = do

  -- Construct a variable from a delta
  let p d = Var (d,False)

  -- Some non-recursive cases
  putStrLn $ "nonrecursive1 :: " ++ show (typeOf (Rec (\_ -> p Any)))
  putStrLn $ "nonrecursive2 :: " ++ show (typeOf (Rec (\_ -> p Leq)))
  putStrLn $ "nonrecursive3 :: " ++ show (typeOf (Rec (\_ -> p Less)))
  putStrLn $ "nonrecursive4 :: " ++ show (typeOf (Rec (\_ -> try (p Less))))

  -- Trivial forms of divergence (all False, hence)
  putStrLn $ "divergence1 :: " ++ show (typeOf (Rec (\v -> Var v)))
  putStrLn $ "divergence2 :: " ++ show (typeOf (Rec (\v -> Seq Id (Var v)))) 

  -- Deep identity
  let deep_id = Rec (\v -> All (Var v))
  putStrLn $ "deep_id :: " ++ show (typeOf deep_id)

  -- Traversal schemes
  putStrLn $ "full_bu Any :: " ++ show (typeOf (full_bu (p Any)))
  putStrLn $ "full_bu Leq :: " ++ show (typeOf (full_bu (p Leq)))
  putStrLn $ "full_bu Less :: " ++ show (typeOf (full_bu (p Less)))
  putStrLn $ "full_td Any :: " ++ show (typeOf (full_td (p Any)))
  putStrLn $ "full_td Leq :: " ++ show (typeOf (full_td (p Leq)))
  putStrLn $ "full_td Less :: " ++ show (typeOf (full_td (p Less)))
  putStrLn $ "stop_td Any :: " ++ show (typeOf (stop_td (p Any)))
  putStrLn $ "stop_td Leq :: " ++ show (typeOf (stop_td (p Leq)))
  putStrLn $ "stop_td Less :: " ++ show (typeOf (stop_td (p Less)))
  putStrLn $ "once_bu Any :: " ++ show (typeOf (once_bu (p Any)))
  putStrLn $ "once_bu Leq :: " ++ show (typeOf (once_bu (p Leq)))
  putStrLn $ "once_bu Less :: " ++ show (typeOf (once_bu (p Less)))

  -- Repeat
  putStrLn $ "repeat Any :: " ++ show (typeOf (repeat (p Any)))
  putStrLn $ "repeat Leq :: " ++ show (typeOf (repeat (p Leq)))
  putStrLn $ "repeat Less :: " ++ show (typeOf (repeat (p Less)))

  -- Innermost (nested closures)
  putStrLn $ "innermost Any :: " ++ show (typeOf (innermost (p Any)))
  putStrLn $ "innermost Leq :: " ++ show (typeOf (innermost (p Leq)))
  putStrLn $ "innermost Less :: " ++ show (typeOf (innermost (p Less)))
