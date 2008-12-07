module Main where

------------------------------------------------------------------------------

--- This module generates a "conditional call graph" from a Java file
--- The conditional call graphs is generated in dot format.

------------------------------------------------------------------------------

import Java
import JavaTermInstances
import StrategyLib
import JavaIO
import JavaUtil
import MonadState
import MonadTrans
import MonadRun

--- Main program -------------------------------------------------------------

main
  = do errLn "\n == Generation of Conditional Call Graph for Java == \n"
       javaIOwrap javax
       errLn "\n == Done. ==\n"
  
--- CCG generation -----------------------------------------------------------

javax :: [CompilationUnit] -> IO String
javax cu
  = do edges <- localNameSupply 0 (java2ccg "" cu)
       return (mkGraph "ccg" edges)

-- Use a name supply to generate unique node names
-- Basically follows the Propagage design pattern, where the
--   name of the parent node is the environment.
       
java2ccg :: (Term a, Monad m) => String -> a -> StateT Integer m String
java2ccg parent x
  = applyTU worker x
    where
      worker = allTU' worker
                 `adhocTU` class2ccg
		 `adhocTU` method2ccg
		 `adhocTU` mcall2ccg
		 `adhocTU` for2ccg1
		 `adhocTU` for2ccg2
		 `adhocTU` if2ccg1
		 `adhocTU` if2ccg2
		 `adhocTU` if2ccg3
      class2ccg (Class1 _ ident _ _ body) 
             = do edges <- java2ccg ident body
	          return ((mkNode ident)++(mkEdge parent ident)++edges)
      method2ccg (MethodHeader_MethodBody header body) 
             = do ident <- getMethodName header
	          edges <- java2ccg ident body
	          return ((mkEdge parent ident)++edges)
      mcall2ccg (PrimaryMethodInvocation prim ident args)
             = do edges <- java2ccg parent (prim,args)
	          return ((mkCallEdge parent ident)++edges)
      mcall2ccg (SuperMethodInvocation ident args)
             = do edges <- java2ccg parent args
	          return ((mkCallEdge parent ident)++edges)
      mcall2ccg (NameMethodInvocation name args)
             = do ident <- return (name2str name)
	          edges <- java2ccg parent args
		  return ((mkCallEdge parent ident)++edges)
      for2ccg1 (For_semicolon_semicolon init expr upd stat)
             = do ident  <- supplyName
	          edges  <- java2ccg parent (init,expr,upd)
		  edges' <- java2ccg ident stat
		  return ((mkIterEdge parent ident)++edges++edges')
      for2ccg2 (For_semicolon_semicolon1 init expr upd stat)
             = do ident  <- supplyName
	          edges  <- java2ccg parent (init,expr,upd)
		  edges' <- java2ccg ident stat
		  return ((mkIterEdge parent ident)++edges++edges') 
      if2ccg1 (If expr stat_t)
             = do ident <- supplyName
	          edges <- java2ccg parent expr
		  es_t  <- java2ccg (ident++":true") stat_t
		  return ((mkCondEdge ["true"] parent ident)
		          ++edges++es_t)
      if2ccg2 (If_else expr stat_t stat_e)
             = do ident <- supplyName
	          edges <- java2ccg parent expr
		  es_t  <- java2ccg (ident++":true") stat_t
		  es_e  <- java2ccg (ident++":false") stat_e
		  return ((mkCondEdge ["true","false"] parent ident)
		          ++edges++es_t++es_e)
      if2ccg3 (If_else1 expr stat_t stat_e)
             = do ident <- supplyName
	          edges <- java2ccg parent expr
		  es_t  <- java2ccg (ident++":true") stat_t
		  es_e  <- java2ccg (ident++":false") stat_e
		  return ((mkCondEdge ["true","false"] parent ident)
		          ++edges++es_t++es_e)

--- Helpers for generating new names -----------------------------------------

supplyName :: MonadState Integer m => m String
supplyName 
  = do i <- get
       put (i+1)
       return ("__"++(show i))

localNameSupply :: Monad m => Integer -> StateT Integer m a -> m a
localNameSupply s
  = unlift (StateAlg s fst)
	
--- Extraction of identifiers from Java fragments ----------------------------

-- Extract base identifier from full name
		  
name2str (Identifier_p idents) = last idents
name2str (Class idents) = last idents

    
--- Helpers for dot generation -----------------------------------------------

mkGraph n edges
  = "digraph "++n++" {\n"++edges++"}\n"

mkNode n    = n++" [ shape=box ]\n"
       
mkEdge "" t = ""    -- This case takes care of root
mkEdge s t  = s++" -> "++t++"\n"

mkCallEdge "" t = ""    -- This case takes care of root
mkCallEdge s t  = s++" -> "++t++" [ style=dashed ]\n"

mkIterEdge "" t = ""    -- This case takes care of root
mkIterEdge s t  = t++" [shape=circle,color=blue,label=\"\",size=.5]\n"++
                  s++" -> "++t++"\n"
		  
mkCondEdge ps "" t = ""    -- This case takes care of root
mkCondEdge ps s t  
  = t++" [shape=record,label="++label++",size=.125,color=brown]\n"++
    s++" -> "++t++"\n"
    where label  = "\"{ if |{ "++ports++" }}\""
          ports  = concat (sepWith " | " (map port ps))
          port p = "<"++p++"> "++p 

------------------------------------------------------------------------------

  
