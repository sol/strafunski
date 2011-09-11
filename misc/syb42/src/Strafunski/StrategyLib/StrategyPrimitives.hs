{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE Rank2Types #-}

-- A simple model of Strafunksi-like strategies.
-- We simply redirect to SYB for extension and traversal.
-- The Q type is done differently from the original presentation.
-- That is, we don't hardwire the monad TC into the Q synonym.

module StrategyLib.StrategyPrimitives (
 T,
 Q,
 idT,
 failT,
 failQ,
 constQ,
 sequT,
 bothQ,
 choiceT,
 choiceQ,
 allT,
 oneT,
 allQ,
 adhocT,
 adhocQ
) where

import Control.Monad
import Data.Generics hiding (choiceQ)

-- Transformations

type T m = forall x. Data x => x -> m x

-- Transformation combinators

idT :: Monad m => T m
failT :: MonadPlus m  => T m
sequT :: Monad m => T m -> T m -> T m
choiceT :: MonadPlus m  => T m -> T m -> T m
allT :: Monad m => T m -> T m
oneT :: MonadPlus m => T m -> T m
adhocT :: (Typeable x, Monad m) => T m -> (x -> m x) -> T m

idT = return
failT = const mzero
sequT f g x = f x >>= g
choiceT f g x = f x `mplus` g x
allT f = gmapM f
oneT f = gmapMo f
adhocT s f = s `extM` f


-- Queries

type Q r = forall x. Data x => x -> r

-- Query combinators

constQ :: r -> Q r
failQ :: MonadPlus m  => Q (m r)
bothQ :: Q u -> Q u' -> Q (u,u')
choiceQ :: MonadPlus m  => Q (m r) -> Q (m r) -> Q (m r)
allQ :: Q r -> Q [r]
adhocQ :: Typeable x => Q r -> (x -> r) -> Q r

constQ r = const r
failQ = const mzero
bothQ f g x = (f x, g x)
choiceQ f g x = f x `mplus` g x
allQ f = gmapQ f
adhocQ s f = s `extQ` f
