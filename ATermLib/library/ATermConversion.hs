{-# OPTIONS -fglasgow-exts -fallow-undecidable-instances -fallow-overlapping-instances #-}

-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Joost Visser 2004
-- License     :  LGPL
-- 
-- Maintainer  :  joost.visser@di.uminho.pt
-- Stability   :  experimental
-- Portability :  portable
--
-- This module is part of the ATerm library for Haskell. It provides the class
-- ATermConvertible of types that are convertible to and from ATerms. Additionally,
-- it provides default instances of this class for some predefined Prelude
-- types.
--
-----------------------------------------------------------------------------

module ATermConversion where

import ATermAbstractSyntax
import ATermReadWrite
import Ratio
import Char

-----------------------------------------------------------------------------
-- * Conversion to and from ATerms

class ATermConvertible t where
  -- | Convert to an ATerm.
  toATerm   :: t -> ATerm
  -- | Convert from an ATerm.
  fromATerm :: ATerm -> t 

-- | Auxiliary function for reporting errors.
fromATermError :: String -> ATerm -> a
fromATermError t u 
  = error ("Cannot convert ATerm to "++t++": "++(err u))
    where err u = case u of 
		  AAppl s _ -> '!':s
		  AList _   -> "!AList"
		  otherwise -> "!AInt"
  
-----------------------------------------------------------------------------
-- * Conversion of ATerms to and from Strings

-- | Convert to a textual ATerm representation without sharing (TXT format).
toATermString 		:: ATermConvertible t => t -> String
toATermString t		=  writeATerm (toATerm t)

-- | Convert to a textual ATerm representation with full sharing (TAF format).
toSharedATermString 	:: ATermConvertible t => t -> String
toSharedATermString t	= writeSharedATerm (toATerm t)

-- | Convert from a textual ATerm representation.
fromATermString 	:: ATermConvertible t => String -> t
fromATermString	s 	= fromATerm (readATerm s)

-----------------------------------------------------------------------------
-- * Instances for basic types
                                                                      -- Lists
instance ATermConvertible a => ATermConvertible [a] where
  toATerm as 		= AList (map toATerm as)
  fromATerm (AList as)	= map fromATerm as
  fromATerm t		= fromATermError "Prelude.[]" t

                                                                      -- Maybe
instance ATermConvertible a => ATermConvertible (Maybe a) where
  toATerm Nothing		= AAppl "None" []
  toATerm (Just a)  		= AAppl "Just" [toATerm a]
  fromATerm (AAppl "None" [])	= Nothing
  fromATerm (AAppl "Some" [a])	= Just (fromATerm a)
  fromATerm t			= fromATermError "Prelude.Maybe" t
  
                                                                   -- 2-Tuples
instance (ATermConvertible a, ATermConvertible b) 
      => ATermConvertible (a,b) where
  toATerm (a,b)			    = AAppl "Tuple2" [toATerm a, toATerm b]
  fromATerm (AAppl "Tuple2" [a,b])  = (fromATerm a,fromATerm b)
  fromATerm t			    = fromATermError "Prelude.(,)" t
  
                                                                     -- Either
instance (ATermConvertible a, ATermConvertible b) 
      => ATermConvertible (Either a b) where
  toATerm (Left a)		    = AAppl "Left" [toATerm a]
  toATerm (Right b)		    = AAppl "Right" [toATerm b]
  fromATerm (AAppl "Left" [a])      = Left (fromATerm a)
  fromATerm (AAppl "Right" [b])     = Right (fromATerm b)
  fromATerm t			    = fromATermError "Prelude.Either" t
  
                                                                     -- String
instance ATermConvertible String where
  toATerm s			= AAppl (show s) []
  fromATerm (AAppl s [])  	= read s
  fromATerm t			= fromATermError "Prelude.String" t

                                                                   -- Integral
instance Integral n => ATermConvertible n where
  toATerm i			= AInt (toInteger i)
  fromATerm (AInt i)  		= fromInteger i
  fromATerm t			= fromATermError "Prelude.Integral" t

                                                                       -- Bool
instance ATermConvertible Bool where
  toATerm True			= AAppl "True" []
  toATerm False			= AAppl "False" []
  fromATerm (AAppl "True" [])	= True
  fromATerm (AAppl "False" [])	= False
  fromATerm t			= fromATermError "Prelude.Bool" t
                                                                       -- Bool

instance ATermConvertible () where
  toATerm ()			= AAppl "()" []
  fromATerm (AAppl "()" [])	= ()
  fromATerm t			= fromATermError "Prelude.()" t

                                                                       -- Char
instance ATermConvertible Char where
  toATerm c			= AInt (toInteger . ord $ c)
  fromATerm (AInt c)		= chr . fromInteger $ c
  fromATerm t			= fromATermError "Prelude.Char" t

                                                                      -- Ratio
instance (Integral a, ATermConvertible a) 
      => ATermConvertible (Ratio a) where
  toATerm xy	= AAppl "Ratio" [toATerm (numerator xy), 
                                 toATerm (denominator xy)]
  fromATerm (AAppl "Ratio" [x,y])
		= (fromATerm x)%(fromATerm y)
  fromATerm t	= fromATermError "Ratio.Ratio" t
  
------------------------------------------------------------------------------
  
