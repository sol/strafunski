------------------------------------------------------------------------------ 
-- | 
-- Maintainer	: Joost Visser
-- Stability	: experimental
-- Portability	: portable
--
-- This module is part of 'Sdf2Haskell', a tool for generating Haskell
-- code from an SDF grammar. This module contains helper functions 
-- for the conversion from Haskell to SDF.

------------------------------------------------------------------------------

module Sdf2HaskellUtils where

import SdfLib
import HaskellLib
import ATermLib (dehyphen)

------------------------------------------------------------------------------

-- | Type synonym for functions that create a Haskell declaration
--   from a type name, a constructor name, and a list of Sdf symbols.
type MkDecl
  = HsName -> String -> [Symbol] -> HsDecl

-- | Convert a context-free SDF production to a Haskell declaration.
production2decl :: MkDecl -> Production -> Maybe HsDecl
production2decl mkDecl prod
  = case (getConsAttr (getAttributes prod),
          sort2hsname (getSort prod),
          isRejectOrBracket prod) 
    of
      (Just str,Just hsname,False) 
         -> Just (mkDecl hsname str (getSyms prod))
      _  -> Nothing

-- | Convert an Sdf sort into a Haskell name
sort2hsname :: Symbol -> Maybe HsName
sort2hsname (Sdf_sort str) = Just (HsIdent (dehyphen str))
sort2hsname _              = Nothing

-- | Convert an Sdf sort into a String 
sort2string :: Symbol -> Maybe String
sort2string (Sdf_sort str) = Just $ dehyphen str
sort2string _              = Nothing

------------------------------------------------------------------------------
-- * General utilities

-- | Generate variables
variables :: [String]
variables
  = map (\i -> '_':(show i)) [0..]

-- | Remove Nothings, and keep values inside Justs.
justFilter :: [Maybe a] -> [a]
justFilter = foldr (\ma as -> maybe as (:as) ma) []


splitBy :: Char -> String -> [String]
splitBy _ [] = []
splitBy split string = first:(splitBy split rest)
   where
      first      = takeWhile (/= split) string
      rest       = dropWhile (== split) afterFirst
      afterFirst = dropWhile (/= split) string

-----------------------------------------------------------------------------
