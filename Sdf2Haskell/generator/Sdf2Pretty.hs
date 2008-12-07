------------------------------------------------------------------------------ 
-- | 
-- Maintainer   : Joost Visser
-- Stability    : experimental
-- Portability  : portable
--
-- This module is part of 'Sdf2Haskell', a tool for generating Haskell
-- code from an SDF grammar. This module contains functions generating
-- pretty-print support.

------------------------------------------------------------------------------

module Sdf2Pretty where

import SdfLib
import HaskellLib
import List(nub)
import Sdf2HaskellUtils
import Sdf2Syntax (symbol2bangtype)

------------------------------------------------------------------------------
-- * Starting point

-- | Generate a Haskell model with functions for pretty-printing support
generatePrettyModule :: String -> String -> SDF -> HsModule
generatePrettyModule name moduleName sdf
  = mkModule moduleName (prettyImports name) (genPrettyDecls name sdf)
       
------------------------------------------------------------------------------
-- * Generation of imports

-- | Generate the import list
prettyImports :: String -> [HsImportDecl]
prettyImports name
  = mkImports [ name,
                name++"TermInstances",
                "Text.PrettyPrint.HughesPJ",
                "GPP",
                "StrategyLib"
              ]
              
------------------------------------------------------------------------------
-- * Generation of function definitions

-- | Generate both instance declarations and the generic pretty-printing
--   function.
genPrettyDecls :: String -> SDF -> [HsDecl]
genPrettyDecls name sdf
  = (genInstDecls sdf)++(genPrettyFun name sdf)

-- | Generate the generic pretty-printing function.
genPrettyFun :: String -> SDF -> [HsDecl]
genPrettyFun name sdf
  = [ HsTypeSig noLoc [funName] funType, 
      HsFunBind [HsMatch noLoc funName [HsPVar . HsIdent $ "gpp"] rhs [] ]]
    where
      modName  = head $ reverse $ splitBy '.' name
      funName  = HsIdent ("upp"++modName)
      funType  = HsQualType [] (HsTyCon (UnQual (HsIdent "UPP")))
      rhs      = HsUnGuardedRhs (mkInfixApp mkConst mkAdhoc updates)
      types :: [HsName]
      types    = nub ( (HsIdent "String"):
                       (justFilter (map sort2hsname (collectCfSorts sdf))) )
      updates :: [HsExp]
      updates  = map mkUpd types
      mkConst  = HsParen $ HsApp (mkVar "constTU") (mkVar "empty")
      mkAdhoc  = HsQVarOp . UnQual . HsIdent $ "adhocQ"
      mkUpd :: HsName -> HsExp
      mkUpd hsname
        = HsParen $ HsExpTypeSig noLoc 
            (HsApp (mkVar "pp") (mkVar "gpp")) 
            (HsQualType [] (HsTyApp (HsTyCon (UnQual (HsIdent "MonoPP"))) 
                                    (HsTyCon (UnQual hsname))))

-- | Generate the instance declarations.
genInstDecls :: SDF -> [HsDecl]
genInstDecls sdf
  = mergeDecls mergeInstDecl (concatMap worker (collectCfProductions sdf))
    where
      worker p = maybe [] (:[]) (production2instdecl p)

-- | Generate an instance declaration from an Sdf production
production2instdecl :: Production -> Maybe HsDecl
production2instdecl prod
  = production2decl mkDecl prod
    where
      mkDecl typename consname symbols
        = HsInstDecl noLoc [] 
            (UnQual . HsIdent $ "PP") 
            [HsTyCon . UnQual $ typename] 
            [HsFunBind 
              [HsMatch noLoc (HsIdent "pp") [gppPat,mkPat consname symbols] 
                 (mkRhs symbols) []
            ] ]
      gppPat
        = HsPVar . HsIdent $ "gpp"
      mkPat consname symbols
        = HsPApp (UnQual . HsIdent $ consname) (symbols2pvars symbols)
      mkRhs symbols
        = HsUnGuardedRhs (mkApp "fsep" [HsList (symbols2docs symbols)])

-- | Convert Sdf symbols into Haskell pattern variables.
symbols2pvars :: [Symbol] -> [HsPat]
symbols2pvars symbols
  = concatMap mkVar (zip (map symbol2bangtype symbols) variables)
    where
      mkVar ([],_)     = []
      mkVar (s:_,v)    = [HsPVar . HsIdent $ v]

-- | Convert Sdf symbols into Doc expressions.      
symbols2docs :: [Symbol] -> [HsExp]
symbols2docs symbols
  = map symbol2doc (zip variables symbols)

-- | Convert an Sdf symbol into a Doc expression.
symbol2doc :: (String, Symbol) -> HsExp
symbol2doc (var,s)
  = s2d (HsVar . UnQual . HsIdent $ var) s
    where
      gppExp = HsVar . UnQual . HsIdent $ "gpp"
    
      s2d v (Sdf_label l s)         = s2d v s
      s2d v (Sdf_lit l)             = mkApp "gpp" [lit2hslit l]
      s2d v (Sdf_sort s)            = mkApp "gpp" [v]
      s2d v (Sdf_seq1 s sl)         = mkApp "fsep" [HsList(symbols2docs (s:sl))]
      s2d v (Sdf_opt s)             = mkApp "gppMaybe" [gppExp,v]
      s2d v (Sdf_iter s)            = mkApp "gppList" [gppExp,v]
      s2d v (Sdf_iter_star s)       = mkApp "gppList" [gppExp,v]
      s2d v (Sdf_iter_sep s sep)      = mkApp "gppListSep" [gppExp,sep2hslit sep,v]
      s2d v (Sdf_iter_star_sep s sep) = mkApp "gppListSep" [gppExp,sep2hslit sep,v]
      s2d v (Sdf_alt1 s1 s2)           = mkApp "gppEither" [gppExp,v]

      s2d v x = error ("s2d not defined for "++(take 50 (show x)))
      
      lit2hslit l
        = HsLit . HsString . lit2string $ l
      sep2hslit (Sdf_lit l)   = lit2hslit l
      sep2hslit _             = error "can not handle non-lit separator"

------------------------------------------------------------------------------
