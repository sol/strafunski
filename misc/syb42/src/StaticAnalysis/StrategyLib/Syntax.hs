module StrategyLib.Syntax where

import Prelude hiding (repeat)


-- Substitution-enabled type-preserving strategies

data TP x
 = Id
 | Fail
 | Seq (TP x) (TP x)
 | Choice (TP x) (TP x)
 | Var x
 | Rec (x -> TP x)
 | All (TP x)
 | One (TP x)

full_bu s   = Rec (\x -> Seq (All (Var x)) s)
full_td s   = Rec (\x -> Seq s (All (Var x)))
once_bu s   = Rec (\x -> Choice (One (Var x)) s)
once_td s   = Rec (\x -> Choice s (One (Var x)))
stop_bu s   = Rec (\x -> Choice (All (Var x)) s)
stop_td s   = Rec (\x -> Choice s (All (Var x)))
innermost s = repeat (once_bu s)
try s       = Choice s Id
repeat s    = Rec (\x -> try (Seq s (Var x)))

-- foo s s' = Rec (\x -> Seq s (Rec (\y -> Seq s' (Seq (All x) y))))
-- innermost s = Rec (\x -> try (Seq (Rec (\y -> Choice (One (Var y)) s)) (Var x)))
