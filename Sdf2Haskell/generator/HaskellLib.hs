------------------------------------------------------------------------------ 
-- | 
-- Maintainer	: Joost Visser
-- Stability	: experimental
-- Portability	: portable
--
-- This module is part of 'Sdf2Haskell', a tool for generating Haskell
-- code from an SDF grammar. This module contains functions for manipulating
-- Haskell terms.

------------------------------------------------------------------------------

module HaskellLib (
  module HaskellLib,
  module Language.Haskell.Syntax
) where

import Language.Haskell.Syntax

------------------------------------------------------------------------------
-- * Templates for code generation

-- | Not used.
dummyModule :: HsModule
dummyModule
  = HsModule noLoc (Module "Dummy") Nothing [] []

-- | Construct a module from its name and list of top-level declarations.
mkModule :: String -> [HsImportDecl] -> [HsDecl] -> HsModule
mkModule name imports decls
  = HsModule noLoc (Module name) Nothing imports decls

-- | Construct a type constructor application from the type constructor
--   name and a list of type arguments.
mkTyApp :: String -> [HsType] -> HsType
mkTyApp c ts
  = foldl (\c t -> HsTyApp c t)
          (mkTyCon c)
          ts

-- | Construct a type constructor from its name.
mkTyCon :: String -> HsType
mkTyCon "[]"	= list_tycon
mkTyCon c	= HsTyCon (UnQual (HsIdent c))

-- | Construct application of a function to several expressions.
mkApp :: String -> [HsExp] -> HsExp
mkApp f es
  = foldl (\f e -> HsApp f e)
          (HsVar . UnQual . HsIdent $ f)
	  es

-- | Construct repeated infix application
mkInfixApp :: HsExp -> HsQOp -> [HsExp] -> HsExp
mkInfixApp e op es
  = foldl (\f e -> HsInfixApp f op e)
          e
	  es

-- | Construct a dummy source location.
noLoc :: SrcLoc
noLoc = SrcLoc "" 0 0

-- | Construct a list of import declarations, given the names of the 
--   modules to be imported.
mkImports :: [String] -> [HsImportDecl]
mkImports moduleNames
  = map mkImportDecl moduleNames
    where
      mkImportDecl moduleName
        = HsImportDecl noLoc (Module moduleName) False Nothing Nothing

-- | Construct a variable.
mkVar :: String -> HsExp
mkVar
  = HsVar . UnQual . HsIdent

------------------------------------------------------------------------------
-- * Merging declarations

-- | Merge Haskell data declarations with a single data constructor each
--   into data declarations with multiple data constructors, by grouping
--   on data type names.
mergeDataDecls :: [HsDecl] -> [HsDecl]
mergeDataDecls ds
  = merge ds []
    where merge []     ds' = ds'
          merge (d:ds) ds' = merge ds (merge1 d ds')
          merge1 d []      = [d]
          merge1 (HsDataDecl _ _ name' _ cds' _) 
                 (HsDataDecl l c name ns cds qns:ds)      
            | name'==name
            = (HsDataDecl l c name ns (cds++cds') qns:ds)
          merge1 d (d':ds) = d':(merge1 d ds)

-- | General function for merging Haskell declarations.
mergeDecls :: (HsDecl -> [HsDecl] -> [HsDecl]) -> [HsDecl] -> [HsDecl]
mergeDecls merge1 ds
  = merge ds []
    where merge []     ds' = ds'
          merge (d:ds) ds' = merge ds (merge1 d ds')

-- | Helper function for merging a single data declaration into
--   a list of declarations. Can be used as first argument for
--   'mergeDecls'.
mergeDataDecl d []	 = [d]
mergeDataDecl (HsDataDecl _ _ name' _ cds' _) 
              (HsDataDecl l c name ns cds qns:ds)	
  | name'==name
  = (HsDataDecl l c name ns (cds++cds') qns:ds)
mergeDataDecl d (d':ds) = d':(mergeDataDecl d ds)

-- | Helper function for merging a single instance declaration into
--   a list of declarations. Can be used as first argument for
--   'mergeDecls'.
mergeInstDecl d []	 = [d]
mergeInstDecl (HsInstDecl _ _ name' ts' fds') 
              (HsInstDecl l c name  ts  fds:ds)	
  | name'==name && ts'==ts
  = (HsInstDecl l c name ts (fds++fds'):ds)
mergeInstDecl d (d':ds) = d':(mergeInstDecl d ds)

------------------------------------------------------------------------------
