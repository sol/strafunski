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

full_td   :: Monad m     => T m -> T m
full_bu   :: Monad m     => T m -> T m
once_td   :: MonadPlus m => T m -> T m
once_bu   :: MonadPlus m => T m -> T m
stop_td   :: MonadPlus m => T m -> T m
innermost :: MonadPlus m => T m -> T m
repeat    :: MonadPlus m => T m -> T m
try       :: MonadPlus m => T m -> T m

full_td s   = s `sequT` allT (full_td s)
full_bu s   = allT (full_bu s) `sequT` s
once_td s   = s `choiceT` oneT (once_td s)
once_bu s   = oneT (once_bu s) `choiceT` s
stop_td s   = s `choiceT` allT (stop_td s)
innermost s = repeat (once_bu s)
repeat s    = try (s `sequT` repeat s)
try s       = s `choiceT` idT

-- Query each node and collect all results in a list
full_cl :: Monoid u => Q u -> Q u
full_cl s = mconcat
          . uncurry (:)
          . bothQ s (allQ (full_cl s))

-- Collection with stop
stop_cl :: Monoid u => Q (Maybe u) -> Q u
stop_cl s = maybe mempty id
          . (s `choiceQ` ( Just
                         . mconcat
                         . allQ (stop_cl s)))

-- Find a node to query in top-down, left-to-right manner
once_cl :: MonadPlus m => Q (m u) -> Q (m u)
once_cl s = s `choiceQ` (msum . allQ (once_cl s))
