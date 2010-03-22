module StrategyLib.Signature where

import Prelude hiding (map,filter)
import Data.Set hiding (map,filter)
import qualified Prelude
import qualified Data.Set as Set


-- Representation of signatures

type Sort      = String
type Constr    = String
type Symbol    = (Constr,[Sort],Sort)
data Signature = Signature { sorts   :: Set Sort
                           , symbols :: Set Symbol
                           }


-- Extract all constructors

constrs :: Signature -> Set Constr
constrs = Set.map (\(c,_,_) -> c) . symbols


-- Look up symbol by constructor

symbol :: Signature -> Constr -> Symbol
symbol sig c = head syms'
 where
  syms = Set.toList $ symbols sig
  syms' = Prelude.filter (\(c',_,_) -> c==c') syms


-- Look up sort of constructor

sortOf :: Signature -> Constr -> Sort
sortOf sig c = so
 where
  (_,_,so) = symbol sig c

argSortsOf :: Signature -> Constr -> Set Sort
argSortsOf sig c = Set.fromList sos
 where
  (_,sos,_) = symbol sig c


-- Return all symbols of a certain result type

symbolsOfSort :: Signature -> Sort -> Set Symbol
symbolsOfSort sig so
 = Set.filter (\sy@(_,_,so') -> so==so')
 $ symbols sig


-- Return all immediate subterm sorts for given symbols

argSortsOfSymbols :: Set Symbol -> Set Sort
argSortsOfSymbols
 = fromList
 . concat
 . Prelude.map (\(_,sos,_) -> sos) 
 . toList


-- Return all immediate subterm sorts for a given sort

argSortsOfSort :: Signature -> Sort -> Set Sort
argSortsOfSort sig
 = argSortsOfSymbols
 . symbolsOfSort sig


-- Test a signature to be well-formed.
-- Note: there is given set of "predefined" sorts.

wellFormed :: Signature -> Set Sort -> Bool
wellFormed sig givenSorts
 = 
     isSubsetOf givenSorts (sorts sig)
  && isSubsetOf useSorts (sorts sig)
  && intersection resSorts givenSorts == empty
  && givenSorts == (difference (sorts sig) resSorts)

 where
  useSorts     = argSorts `union` resSorts
  resSorts     = Set.map (\(_,_,so) -> so) $ symbols sig
  argSorts     = argSortsOfSymbols (symbols sig)
