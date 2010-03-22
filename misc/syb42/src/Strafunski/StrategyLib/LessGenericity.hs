{-# LANGUAGE Rank2Types #-}

module StrategyLib.LessGenericity where

import Data.Data
import Control.Monad
import StrategyLib.Baseline

full_td'   :: (Data x, Data y, Monad m) => (x -> m x) -> y -> m y
full_bu'   :: (Data x, Data y, Monad m) => (x -> m x) -> y -> m y
once_td'   :: (Data x, Data y, MonadPlus m) => (x -> m x) -> y -> m y
once_bu'   :: (Data x, Data y, MonadPlus m) => (x -> m x) -> y -> m y
stop_td'   :: (Data x, Data y, MonadPlus m) => (x -> m x) -> y -> m y
innermost' :: (Data x, Data y, MonadPlus m) => (x -> m x) -> y -> m y

full_td' s     = full_td (adhocTP idTP s)
full_bu' s     = full_bu (adhocTP idTP s)
once_td' s     = once_td (adhocTP failTP s)
once_bu' s     = once_bu (adhocTP failTP s)
stop_td' s     = stop_td (adhocTP failTP s)
innermost' s   = innermost (adhocTP failTP s)
