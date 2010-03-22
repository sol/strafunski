{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE Rank2Types #-}

-- A simple model of Strafunksi-like strategies.
-- We simply redirect to SYB for extension and traversal.
-- The TU type is done differently from the original presentation.
-- That is, we don't hardwire the monad TC into the TU synonym.

module StrategyLib.StrategyPrimitives (
 TP,
 TU,
 idTP,
 failTP,
 failTU,
 constTU,
 sequTP,
 bothTU,
 choiceTP,
 choiceTU,
 allTP,
 oneTP,
 allTU,
 adhocTP,
 adhocTU
) where

import Control.Monad
import Data.Generics

-- Type-preserving and -unifying strategies
type TP m = forall x. Data x => x -> m x
type TU r = forall x. Data x => x -> r

-- Basis combinators
idTP     :: Monad m      => TP m
failTP   :: MonadPlus m  => TP m
failTU   :: MonadPlus m  => TU (m r)
constTU  ::                 r -> TU r
sequTP   :: Monad m      => TP m -> TP m -> TP m
bothTU   ::                 TU u -> TU u' -> TU (u,u')
choiceTP :: MonadPlus m  => TP m -> TP m -> TP m
choiceTU :: MonadPlus m  => TU (m r) -> TU (m r) -> TU (m r)

-- One-layer traversal combinators
allTP    :: Monad m           => TP m -> TP m
oneTP    :: MonadPlus m       => TP m -> TP m
allTU    ::                      TU r -> TU [r]

-- Strategy extension
adhocTP  :: (Typeable x, Monad m) => TP m -> (x -> m x) -> TP m
adhocTU  :: Typeable x            => TU r -> (x -> r) -> TU r


-- Implementations
idTP     = return
failTP   = const mzero
failTU   = const mzero
constTU  = \r -> const r
sequTP   = \f g x -> f x >>= g
bothTU   = \f g x -> (f x, g x)
choiceTP = \f g x -> f x `mplus` g x
choiceTU = \f g x -> f x `mplus` g x
allTP    = \f -> gmapM f
oneTP    = \f -> gmapMo f
allTU    = \f -> gmapQ f
adhocTP  = \g s -> g `extM` s
adhocTU  = \g s -> g `extQ` s
