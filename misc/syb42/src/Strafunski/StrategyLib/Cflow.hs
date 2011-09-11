{-# LANGUAGE Rank2Types #-}

module StrategyLib.Cflow where

import Prelude hiding (repeat)
import Data.Generics
import Data.Maybe
import Control.Monad
import Control.Monad.Maybe -- requires "cabal install MaybeT"

-- Transformations

newtype Monad m => T m = T { getT :: forall x. Data x => x -> m x }
newtype Monad m => T' m = T' { getT' :: forall x. Data x => x -> MaybeT m x }
t2t' :: Monad m => T m -> T' m
t2t' f = T' (\x -> MaybeT (getT f x >>= return . Just))


-- Primitives

idT :: Monad m => T m
idT' :: Monad m => T' m
failT :: Monad m  => T' m
sequT :: Monad m => T m -> T m -> T m
sequT' :: Monad m => T' m -> T' m -> T' m
choiceT :: Monad m  => T' m -> T m -> T m
choiceT' :: Monad m  => T' m -> T' m -> T' m
allT :: Monad m => T m -> T m
allT' :: Monad m => T' m -> T' m
oneT :: Monad m => T m -> T' m
oneT' :: Monad m => T' m -> T' m
adhocT :: (Typeable x, Monad m) => T m -> (x -> m x) -> T m
adhocT' :: (Typeable x, Monad m) => T' m -> (x -> MaybeT m x) -> T' m

idT = T return
idT' = t2t' idT
failT = T' (const (MaybeT (return Nothing)))
sequT f g = T (\x -> getT f x >>= getT g)
sequT' f g = T' (\x -> getT' f x >>= getT' g)
choiceT f g = T (\x -> runMaybeT (getT' f x) >>= maybe (getT g x) return)
choiceT' f g = T' (\x -> getT' f x `mplus` getT' g x)
allT f = T (gmapM (getT f))
allT' f = undefined -- left as an exercise
oneT f = undefined -- left as an exercise
oneT' f = undefined -- left as an exercise
adhocT s f = T ((getT s) `extM` f)
adhocT' s f = T' ((getT' s) `extM` f)

full_td   :: Monad m => T m -> T m
full_bu   :: Monad m => T m -> T m
once_td   :: Monad m => T' m -> T' m
once_bu   :: Monad m => T' m -> T' m
stop_td   :: Monad m => T' m -> T m
innermost :: Monad m => T' m -> T m
repeat    :: Monad m => T' m -> T m
try       :: Monad m => T' m -> T m

full_td s    = s `sequT` allT (full_td s)
full_bu s    = allT (full_bu s) `sequT` s
once_td s    = s `choiceT'` oneT' (once_td s) 
once_bu s    = oneT' (once_bu s) `choiceT'` s
stop_td s    = s `choiceT` allT (stop_td s)
innermost s  = repeat (once_bu s)
repeat s     = try (s `sequT'` t2t' (repeat s))
try s        = s `choiceT` idT
