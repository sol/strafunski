{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module StrategyLib.Reachability where

import Data.Data
import StrategyLib.Baseline
import StrategyLib.Default


-- The type-level for reachability

class ReachableFrom x y
instance ReachableFrom x x
instance ReachableFrom x [x]


-- A traversal scheme with reachability constraint

full_td'' :: (Data x, Data y, Monad m, ReachableFrom x y)
          => (x -> m x)
          -> y -> m y
full_td'' = full_td'
