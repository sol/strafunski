module AnalysisLib.Reachability where

-- Reachability analysis for type-specific cases

import Data.Set hiding (map,fromList,toList)
import qualified Data.Set as Set
import SemanticsLib.Domain
import SemanticsLib.Map hiding (fromList,toList,lookup,update)
import qualified SemanticsLib.Map as Map
import SemanticsLib.Fix
import StrategyLib.Syntax
import StrategyLib.Signature


-- Abstract type-specific cases

type Case  = String -- "Id"
type Cases = Set Case


-- The abstract domain for strategies

type Abs = Map Sort Cases


-- The actual analysis

analyse :: Signature -> TP Abs -> Abs
analyse sig = analyse' 
 where
  analyse' :: TP Abs -> Abs
  analyse' Id            = bottom
  analyse' Fail          = bottom
  analyse' (Seq s s')    = analyse' s `lub` analyse' s'
  analyse' (Choice s s') = analyse' s `lub` analyse' s'
  analyse' (Var x)       = x
  analyse' (Rec f)       = fixEq (analyse' . f)
  analyse' (All s)       = transform sig $ analyse' s
  analyse' (One s)       = transform sig $ analyse' s


-- One-layer traversal

transform :: Signature -> Abs -> Abs
transform sig abs
 = Map.fromList 
 $ map perSort 
 $ Set.toList 
 $ sorts sig
 where
  perSort :: Sort -> (Sort, Cases)
  perSort so = (so,cases)
   where
    cases = lubs
          $ map perArgSort
          $ Set.toList argSorts
     where
      argSorts = argSortsOfSort sig so
      perArgSort = flip Map.lookup abs
