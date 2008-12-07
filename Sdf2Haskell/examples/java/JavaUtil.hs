module JavaUtil where

import Java
import JavaATermConvertibleInstances
import JavaTermInstances
import ChaseImports
import StrategyLib
import Monad

-- Extract method name from its header

getMethodName header
  = case (applyTU (once_buTU (failTU `adhocTU` worker)) header) of
      Just ident  -> return ident
      Nothing     -> error "Method without a name?"
    where
      worker (Comma2 ident _) = return ident
      worker _                = mzero

-- Fully qualified class name as string

qname2str (Identifier_p idents) = concat (sepWith "." idents)
qname2str (Class idents)        = concat (sepWith "." ("class":idents))

--- Auxilliaries -------------------------------------------------------------

quote s = "\""++s++"\""

sepWith :: a -> [a] -> [a]
sepWith _ [] = []
sepWith a [x] = [x]
sepWith a (x:xs) = x:a: sepWith a xs

-------------------------------------------------------------------------------
