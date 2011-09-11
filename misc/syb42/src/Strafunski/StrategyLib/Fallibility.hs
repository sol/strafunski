{-# LANGUAGE Rank2Types #-}

module StrategyLib.Fallibility where

import Prelude hiding (repeat)
import StrategyLib.Baseline
import Control.Monad.Maybe -- requires "cabal install MaybeT"

-- Simple approach: elimination of monad parameter

full_td'   :: T Id -> T Id
full_bu'   :: T Id -> T Id
stop_td'   :: T Maybe -> T Id
innermost' :: T Maybe -> T Id
repeat'    :: T Maybe -> T Id
try'       :: T Maybe -> T Id

full_td' s    = full_td s
full_bu' s    = full_bu s
stop_td' s    = s `choiceT'` allT (stop_td' s)
innermost' s  = repeat' (once_bu s)
repeat' s     = try' (s `sequT'` repeat' s)
try' s        = s `choiceT'` idT

choiceT' :: T Maybe -> T Id -> T Id
choiceT' f g x = maybe (g x) Id (f x)

sequT' :: T Maybe -> T Id -> T Maybe
sequT' f g = f `sequT` (Just . getId . g)


-- Advanced approach: preservation of monad parameter

full_td''   :: Monad m => T m -> T m -- equals original type
full_bu''   :: Monad m => T m -> T m -- equals original type
stop_td''   :: Monad m => T (MaybeT m) -> T m
innermost'' :: Monad m => T (MaybeT m) -> T m
repeat''    :: Monad m => T (MaybeT m) -> T m
try''       :: Monad m => T (MaybeT m) -> T m

full_td'' s   = full_td s
full_bu'' s   = full_bu s
stop_td'' s   = s `choiceT''` allT (stop_td'' s)
innermost'' s = repeat'' (once_bu s)
repeat'' s    = try'' (s `sequT''` repeat'' s)
try'' s       = s `choiceT''` idT

choiceT'' :: Monad m => T (MaybeT m) -> T m -> T m
choiceT'' f g x = runMaybeT (f x) >>= maybe (g x) return

sequT'' :: Monad m => T (MaybeT m) -> T m -> T (MaybeT m)
sequT'' f g = f `sequT` (MaybeT . flip (>>=) (return . Just) . g)
