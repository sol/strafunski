{-# LANGUAGE Rank2Types #-}

module StrategyLib.Infallibility where

import Prelude hiding (repeat)
import StrategyLib.Baseline

full_td'   :: TP Id -> TP Id
full_bu'   :: TP Id -> TP Id
stop_td'   :: TP Maybe -> TP Id
innermost' :: TP Maybe -> TP Id
repeat'    :: TP Maybe -> TP Id
try'       :: TP Maybe -> TP Id

full_td' s    = full_td s
full_bu' s    = full_bu s
stop_td' s    = s `choiceTP'` allTP (stop_td' s)
innermost' s  = repeat' (once_bu s)
repeat' s     = try' (s `sequTP` (Just . getId . repeat' s))
try' s        = s `choiceTP'` Id

choiceTP' :: TP Maybe -> TP Id -> TP Id
choiceTP' f g x = maybe (g x) Id (f x)
