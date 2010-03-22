{-# LANGUAGE Rank2Types #-}

module StrategyLib.Baseline (
 module StrategyLib.StrategyPrimitives,
 Id(Id),
 getId,
 full_td,
 full_bu,
 once_td,
 once_bu,
 stop_td,
 stop_bu,
 innermost,
 repeat,
 try,
 full_cl,
 once_cl,
 stop_cl
) where

import Prelude hiding (repeat)
import Data.Monoid
import Control.Monad
import StrategyLib.StrategyPrimitives

newtype Id x = Id { getId :: x }

instance Monad Id
 where
  return = Id
  x >>= f = f (getId x)

full_td   :: Monad m     => TP m -> TP m
full_bu   :: Monad m     => TP m -> TP m
once_td   :: MonadPlus m => TP m -> TP m
once_bu   :: MonadPlus m => TP m -> TP m
stop_td   :: MonadPlus m => TP m -> TP m
stop_bu   :: MonadPlus m => TP m -> TP m
innermost :: MonadPlus m => TP m -> TP m
repeat    :: MonadPlus m => TP m -> TP m
try       :: MonadPlus m => TP m -> TP m

full_td s   = s `sequTP` allTP (full_td s)
full_bu s   = allTP (full_bu s) `sequTP` s
once_td s   = s `choiceTP` oneTP (once_td s)
once_bu s   = oneTP (once_bu s) `choiceTP` s
stop_td s   = s `choiceTP` allTP (stop_td s)
stop_bu s   = allTP (stop_bu s) `choiceTP` s
innermost s = repeat (once_bu s)
repeat s    = try (s `sequTP` repeat s)
try s       = s `choiceTP` idTP

-- Query each node and collect all results in a list
full_cl :: Monoid u => TU u -> TU u
full_cl s = mconcat
          . uncurry (:)
          . bothTU s (allTU (full_cl s))

-- Collection with stop
stop_cl :: Monoid u => TU (Maybe u) -> TU u
stop_cl s = maybe mempty id
          . (s `choiceTU` ( Just
                          . mconcat
                          . allTU (stop_cl s)))

-- Find a node to query in top-down, left-to-right manner
once_cl :: MonadPlus m => TU (m u) -> TU (m u)
once_cl s = s `choiceTU` ( msum . allTU (once_cl s))
