{-# LANGUAGE Rank2Types #-}

module StrategyLib.Default where

import Data.Data
import Control.Monad
import StrategyLib.Baseline

full_td'   :: (Data x, Data y, Monad m) => (x -> m x) -> y -> m y
full_bu'   :: (Data x, Data y, Monad m) => (x -> m x) -> y -> m y
once_td'   :: (Data x, Data y, MonadPlus m) => (x -> m x) -> y -> m y
once_bu'   :: (Data x, Data y, MonadPlus m) => (x -> m x) -> y -> m y
stop_td'   :: (Data x, Data y, MonadPlus m) => (x -> m x) -> y -> m y
innermost' :: (Data x, Data y, MonadPlus m) => (x -> m x) -> y -> m y

full_td' f     = full_td (adhocT idT f)
full_bu' f     = full_bu (adhocT idT f)
once_td' f     = once_td (adhocT failT f)
once_bu' f     = once_bu (adhocT failT f)
stop_td' f     = stop_td (adhocT failT f)
innermost' f   = innermost (adhocT failT f)
