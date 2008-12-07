------------------------------------------------------------------------------ 
-- | 
-- Maintainer	: Joost Visser
-- Stability	: experimental
-- Portability	: portable
--
-- This module is part of 'Sdf2Haskell', a tool for generating Haskell
-- code from an SDF grammar. This module contains functions generating
-- abstract syntax types.

------------------------------------------------------------------------------

module Sdf2Cfg --( --genCfgDecl
--) 
where

import Char
import List

import Cfg
import Sdf2HaskellUtils
import SdfLib
import HaskellLib
import List(nub)
import ATermLib -- (toATermString) -- For debugging

import Debug.Trace






instance (ATermConvertible t,
          ATermConvertible nt) => ATermConvertible (Cfg t nt) where
    toATerm (Cfg aa ab ac ad) = (AAppl "Cfg" [ toATerm aa,toATerm ab,toATerm ac,toATerm ad ])
    fromATerm (AAppl "Cfg" [ aa,ab,ac,ad ]) = let aa' = fromATerm aa
                                                  ab' = fromATerm ab
                                                  ac' = fromATerm ac
                                                  ad' = fromATerm ad in (Cfg aa' ab' ac' ad')
    fromATerm u = fromATermError "Cfg" u

instance (ATermConvertible t,
          ATermConvertible nt) => ATermConvertible (Symb t nt) where
    toATerm Dollar = (AAppl "Dollar" [ ])
    toATerm Root = (AAppl "Root" [ ])
    toATerm (T aa) = (AAppl "T" [ toATerm aa ])
    toATerm (NT aa) = (AAppl "NT" [ toATerm aa ])
    fromATerm (AAppl "Dollar" [ ]) = let in Dollar
    fromATerm (AAppl "Root" [ ]) = let in Root
    fromATerm (AAppl "T" [ aa ]) = let aa' = fromATerm aa in (T aa')
    fromATerm (AAppl "NT" [ aa ]) = let aa' = fromATerm aa in (NT aa')
    fromATerm u = fromATermError "Symb" u


completeCfg :: [CfgProd] -> ([Symb Char String],[Symb Char String])
completeCfg ps = (nub (filter (not.(`elem` defined)) used),nub (filter (not.(`elem` used)) defined))
	where 
          defined = map (lhs_prod.snd) ps
          used    = concat $ map ((filter isNT).rhs_prod.snd) ps
          isNT (NT _) = True
          isNT _ = False

ensureComplete ps = case completeCfg ps of ([],[NT "<Start>"]) -> ps
					   x       -> error $ show x  

------------------------------------------------------------------------------
-- * Starting point 

type CfgProd  = Prod Char String
type CfgSymb  = Symb Char String


genCfgDecl :: SDF -> [CfgProd]
genCfgDecl sdf 
  = ensureComplete $ nub $ (map sdfprod2cfgprod ps)++rangesPs
    where
      ps   = collectProductions nsdf
      nsdf =  sdf
      ranges = collectRanges sdf
      rangesPs = concatMap (ranges2prods) ranges

range2Chars r@(Sdf_range i1 i2)
  = [c1..c2]
    where
      c1 = sdfChar2t i1
      c2 = sdfChar2t i2
range2Chars r@(Sdf_Character i) = [c]
    where c = sdfChar2t i



range2prods r@(Sdf_range i1 i2)
  = [("PRODNAME",[NT (showSdf r),T c]) | c <- [c1..c2]]
    where
      c1 = sdfChar2t i1
      c2 = sdfChar2t i2
range2prods r@(Sdf_Character i) = [("PRODNAME",[NT (showSdf r),T c])]
    where c = sdfChar2t i


ranges2Chars :: CharRanges -> [Char]
ranges2Chars (Sdf_CharRange cr)   = range2Chars cr
ranges2Chars (Sdf_conc crs1 crs2) = ranges2Chars crs1 ++ ranges2Chars crs2


ranges2prods :: CharRanges -> [CfgProd]
ranges2prods crs 
  = [("PRODNAME",[NT (showSdf crs),T c]) | c <- ranges2Chars crs ]

sdfprod2cfgprod :: Production -> CfgProd 
sdfprod2cfgprod prod 
	= (name, nt:syms)
	where 
	name = maybe pname  id $ getConsAttr (getAttributes prod)
	nt   = sort2nt $ getSort prod
        symbols = getSyms prod
	syms = map symbol2sym symbols
	filterChars = filter (\x -> not (x `elem` ['<','>','\\','"','?','_']))
	pname = filterChars (showSymb nt) ++ " -- " ++ filterChars (concatMap showSymb syms) 
	

sort2nt :: Symbol -> CfgSymb
sort2nt = NT . showSdf

--sdfprod2cfgprod (Sdf_prod_fun l ss s ats) = undefined

sdfChar2t :: Character -> Char
sdfChar2t (Sdf_numeric (('\\'):i)) = chr (read i)
sdfChar2t c = error $ "non-handle character " ++ show c


symbol2sym :: Symbol -> CfgSymb
symbol2sym s
  = s2t s
    where
      s2t (Sdf_char_class1 (Sdf_simple_charclass (Sdf_present1 (Sdf_CharRange (Sdf_Character i))))) 
        = T $ sdfChar2t i
      s2t (Sdf_char_class1 (Sdf_simple_charclass (Sdf_present1 r))) 
        = NT $ (showSdf r)
      s2t x                         = sort2nt x

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
  = [HsTypeDecl noLoc (HsIdent str) [] (mkTyCon "String")]
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
      prods = collectProductions sdf
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
      s2t (Sdf_label l s)	    = s2t s
      s2t (Sdf_lit l)		    = []
      s2t (Sdf_sort s)		    = [mkTyCon s]
      s2t (Sdf_seq1 s ss)	    = do t  <- s2t s 
            			         ts <- [concatMap s2t ss]
				         [HsTyTuple (t:ts)]
      s2t (Sdf_opt s)		    = let s' = do t <- s2t s
				                  [mkTyApp "Maybe" [t]]
                                      in case s' of
                                        [] -> [mkTyApp "Maybe" [HsTyTuple []]]
					_  -> s'
      s2t (Sdf_iter s)		    = it2t s
      s2t (Sdf_iter_star s)	    = it2t s
      s2t (Sdf_iter_sep s sep)      = itsep2t s sep
      s2t (Sdf_iter_star_sep s sep) = itsep2t s sep
      s2t (Sdf_iter_n s n)	    = it2t s
      s2t (Sdf_iter_sep_n s sep n)  = itsep2t s sep
      s2t (Sdf_alt1 s1 s2)	    = do t1 <- s2t s1
        			         t2 <- s2t s2
				         [mkTyApp "Either" [t1,t2]]
      --s2t (Sdf_char_class1 CharClass)
      --s2t (Sdf_cf Symbol)
      --s2t (Sdf_layout)
      s2t _ 			    = []

      it2t s		= let s' = do t <- s2t s
    			              [mkTyApp "[]" [t]]
                          in case s' of
                              [] -> [mkTyApp "[]" [HsTyTuple []]]
			      _  -> s'
      itsep2t s sep	= case s2t sep of
                            [] -> do t <- s2t s
                                     [mkTyApp "[]" [t]]
                            _  -> error $ "Can't handle separator: "++(toATermString sep)

------------------------------------------------------------------------------
