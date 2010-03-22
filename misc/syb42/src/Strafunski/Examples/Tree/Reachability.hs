{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Examaples.Tree.Reachability where

import Data.Tree
import StrategyLib.Baseline
import StrategyLib.Reachability


-- Supply metadata for reachability

instance ReachableFrom a (Tree a)
instance ReachableFrom [Tree a] (Tree a)
