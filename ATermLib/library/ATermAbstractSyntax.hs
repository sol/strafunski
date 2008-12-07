-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Joost Visser 2004
-- License     :  LGPL
-- 
-- Maintainer  :  joost.visser@di.uminho.pt
-- Stability   :  experimental
-- Portability :  portable
--
-- This module is part of the ATerm library for Haskell. It defines
-- the abstract syntax of ATerms as a Haskell datatype.
--
-----------------------------------------------------------------------------

module ATermAbstractSyntax (
	ATerm(..)
) where

-----------------------------------------------------------------------------

-- | The abstract syntax of ATerms.
data ATerm = AAppl String [ATerm]	-- ^ Application
           | AList [ATerm]		-- ^ Lists
           | AInt Integer		-- ^ Integers
           deriving (Read,Show,Eq,Ord)

------------------------------------------------------------------------------
