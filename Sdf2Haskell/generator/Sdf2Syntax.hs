------------------------------------------------------------------------------ 
-- | 
-- Maintainer   : Joost Visser
-- Stability    : experimental
-- Portability  : portable
--
-- This module is part of 'Sdf2Haskell', a tool for generating Haskell
-- code from an SDF grammar. This module contains functions generating
-- abstract syntax types.

------------------------------------------------------------------------------

module Sdf2Syntax (
  generateSyntaxModule,
  symbol2bangtype
) where

import Sdf2HaskellUtils
import SdfLib
import HaskellLib
import List(nub)
import ATermLib (toATermString) -- For debugging
import ATermLib (dehyphen)


------------------------------------------------------------------------------
-- * Starting point 

-- | Generate Haskell module with type declarations for 
--   representation of abstract syntax.
generateSyntaxModule :: String -> SDF -> HsModule
generateSyntaxModule name sdf
  = mkModule name [] (genSyntaxDecls sdf)     

-- | Generate type declarations for representation of abstract syntax.
genSyntaxDecls :: SDF -> [HsDecl]
genSyntaxDecls sdf
  = (genDataDecls sdf) ++
    (genTypeDecls sdf)

------------------------------------------------------------------------------
-- * Generate type synonyms from lexical sorts 

-- | Generate Haskell type declarations for all lexical sorts defined
--   in a given SDF grammar.
genTypeDecls :: SDF -> [HsDecl]
genTypeDecls sdf
  = nub (concatMap sort2typedecl (collectLexSorts sdf))

-- | Convert a lexical SDF sort to a Haskell type synonym.
sort2typedecl :: Symbol -> [HsDecl]
sort2typedecl (Sdf_sort str) 
  = [HsTypeDecl noLoc (HsIdent $ dehyphen str) [] (mkTyCon "String")]
sort2typedecl _              
  = []

------------------------------------------------------------------------------
-- * Generate data declarations from context-free productions

-- | Generate Haskell data declarations for all context-free productions
--   in a given SDF grammar.
genDataDecls :: SDF -> [HsDecl]
genDataDecls sdf
  = mergeDecls mergeDataDecl decls
    where
      prods = collectCfProductions sdf
      decls = justFilter (map production2datadecl prods)

-- | Convert a context-free SDF production to a Haskell data declaration
--   with a single data constructor.
production2datadecl :: Production -> Maybe HsDecl
production2datadecl prod
  = production2decl mkDecl prod
    where
      mkDecl typename consname symbols  
        = mkHsDataDecl typename consname (symbols2types symbols)
      mkHsDataDecl hsname str ts
        = HsDataDecl noLoc [] hsname [] 
            [HsConDecl noLoc (HsIdent str) ts] []


-- | Convert Sdf symbols to Haskell types. 
symbols2types :: [Symbol] -> [HsBangType]
symbols2types symbols
  = concatMap symbol2bangtype symbols

-- | Convert an Sdf symbol into Haskell types. The result type is actually
--   a list of Haskell types rather than a single one, though I do not remember
--   why...
symbol2bangtype :: Symbol -> [HsBangType]
symbol2bangtype s
  = do t <- s2t s
       [HsUnBangedTy t]
    where
      s2t (Sdf_label l s)           = s2t s
      s2t (Sdf_lit l)               = []
      s2t (Sdf_sort s)              = [mkTyCon $ dehyphen s]
      s2t (Sdf_seq1 s ss)           = let ts = concatMap s2t (s:ss)
                                      in if (ts == []) 
                                         then [] else [HsTyTuple ts]
      s2t (Sdf_opt s)               = let s' = do t <- s2t s
                                                  [mkTyApp "Maybe" [t]]
                                      in case s' of
                                        [] -> [mkTyApp "Maybe" [HsTyTuple []]]
                                        _  -> s'
      s2t (Sdf_iter s)              = it2t s
      s2t (Sdf_iter_star s)         = it2t s
      s2t (Sdf_iter_sep s sep)      = itsep2t s sep
      s2t (Sdf_iter_star_sep s sep) = itsep2t s sep
      s2t (Sdf_iter_n s n)          = it2t s
      s2t (Sdf_iter_sep_n s sep n)  = itsep2t s sep
      s2t (Sdf_alt1 s1 s2)          = do t1 <- voidOr (s2t s1)
                                         t2 <- voidOr (s2t s2)
                                         [mkTyApp "Either" [t1,t2]]
      --s2t (Sdf_char_class1 CharClass)
      --s2t (Sdf_cf Symbol)
      --s2t (Sdf_layout)
      s2t _                         = []
      
      voidOr [] = [HsTyTuple []]
      voidOr ss = ss

      it2t s            = let s' = do t <- s2t s
                                      [mkTyApp "[]" [t]]
                          in case s' of
                              [] -> [mkTyApp "[]" [HsTyTuple []]]
                              _  -> s'
      itsep2t s sep     = case s2t sep of
                            [] -> do t <- s2t s
                                     [mkTyApp "[]" [t]]
                            _  -> error $ "Can't handle separator: "++(toATermString sep)

------------------------------------------------------------------------------
