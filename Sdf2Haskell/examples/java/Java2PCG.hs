module Main where

-------------------------------------------------------------------------------

--- Extract Package & Class Graph from Java files.
--- This graph shows relations between and among classes and packages,
--- i.e. inheritance, containment, dependence.

import Java
import JavaTermInstances
import StrategyLib
import JavaIO
import Control.Monad.Identity

--- Main program -------------------------------------------------------------

main
  = do errLn "\n == Generation of Package/Class Graph for Java == \n"
       javaIOwrap java2pcg
       errLn "\n == Done. ==  \n"
  
-------------------------------------------------------------------------------

java2pcg 	:: [CompilationUnit] -> IO String
java2pcg cus	=  do edges <- java2pcg' "" cus
                      return (mkGraph "pcg" edges)

java2pcg' :: (Term x) => String -> x -> IO String
java2pcg' parent x
  = applyTU worker x
    where
      worker = allTU' worker
                 `adhocTU` cu2pcg
		 `adhocTU` class2pcg
		 `adhocTU` clsimp2pcg
		 `adhocTU` pkgimp2pcg
		 `adhocTU` type2pcg
      cu2pcg (CU package imports types)
        = do ident  <- return (packDecl2ident package)
	     idents <- collectDeclaredTypes types
	     i_es   <- mapM (\i -> java2pcg' (ident++"."++i) imports) idents
	     edges  <- java2pcg' ident types
	     return ((mkPkg ident)++(concat i_es)++edges)
      class2pcg (Class1 _ classname super interfaces body) 
        = do ident <- return (parent++"."++classname)
	     s_es  <- return (super2pcg ident super)
	     i_es  <- return (interfaces2pcg ident interfaces)
	     edges <- java2pcg' ident body
	     return ((mkCls ident)++(mkNesting parent ident)++s_es++i_es++edges)
      clsimp2pcg (Import_semicolon name)
        = do ident <- return (qname2str name)
	     return ((mkCls ident)++(mkImport parent ident))
      pkgimp2pcg (Import_times_semicolon name)
        = do ident <- return (qname2str name)
	     return ((mkPkg ident)++(mkImport parent ident))
      type2pcg (Name name)
        = return (mkImport parent (qname2str name))
	     
      super2pcg cls super 
        = concatMap (mkInherit cls.qname2str) (collectNames super)
      interfaces2pcg cls interfaces
        = concatMap (mkImplement cls.qname2str) (collectNames interfaces)
      
---

collectDeclaredTypes x
  = applyTU (stop_tdTU worker) x
    where
      worker = failTU `adhocTU` (\(Class1 _ ident _ _ _)  -> return [ident]) 
                      `adhocTU` (\(Interface _ ident _ _) -> return [ident])

packDecl2ident Nothing                         = "NO PACKAGE"
packDecl2ident (Just (Package_semicolon name)) = qname2str name

-- Fully qualified class name as string

qname2str (Identifier_p idents) = concat (sepWith "." idents)
qname2str (Class idents)        = concat (sepWith "." ("class":idents))

mkGraph n edges
  = "digraph "++n++" {\n"++edges++"}\n"

mkNesting "" t = ""    -- This case takes care of root
mkNesting s t  = quote s++" -> "++quote t++"\n"

mkImport "" t = ""    -- This case takes care of root
mkImport s t  = quote s++" -> "++quote t++" [ color=blue, arrowsize=0.5 ]\n"

mkInherit t s  
  = quote s++" -> "++quote t++" [ dir=back ]\n"++mkCls s

mkImplement t s  
  = quote s++" -> "++quote t++" [ dir=back, style=dotted ]\n"++mkIfc s

mkPkg "" = ""
mkPkg p  = quote p++" [ shape=box, style=filled, color=tan ]\n"

mkCls "" = ""
mkCls c  = quote c++" [ shape=box, color=green ]\n"

mkIfc "" = ""
mkIfc i  = quote i++" [ shape=box, color=green, style=dashed ]\n"

collectNames :: Term x => x -> [Name]
collectNames x
  = runIdentity (applyTU (full_tdTU worker) x)
    where
      worker = constTU [] `adhocTU` (\name -> return [name])
	    
--- Auxilliaries -------------------------------------------------------------

quote s = "\""++s++"\""

sepWith :: a -> [a] -> [a]
sepWith _ [] = []
sepWith a [x] = [x]
sepWith a (x:xs) = x:a: sepWith a xs

-------------------------------------------------------------------------------
