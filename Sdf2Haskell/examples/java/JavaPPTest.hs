module Main where

import JavaChaseImports
import JavaPP
import System
import GPP
import ATermLib
import Text.PrettyPrint.HughesPJ
import Java

main
  = do args <- getArgs
       putStrLn $ "//  Arguments given: "++show args
       let name = head args
       maybeCU <- parseJavaFileOnPath ["."] name
       case maybeCU of
         (Right msg)    -> putStrLn $ "Error: "++msg
	 (Left cu)      -> do --putStrLn "As ATerm..."
	                      --putStrLn $ toATermString cu
	                      putStrLn "//  Rendering ..."
	                      putStrLn . renderJava $ cu

renderJava x
  = renderFix upp x
    where
      upp :: UPP
      upp gpp
        = (uppJava gpp)
	    `adhocQ` (ppBlock gpp)
	    `adhocQ` (ppCatchClause gpp)
	    `adhocQ` (ppClassDeclaration gpp)
      ppBlock :: GPP -> Block -> Doc
      ppBlock gpp (BlockStatement_s _1)
	  = sep [gpp "{", nest 2 (gppList gpp _1), gpp "}"]
	  
      ppCatchClause :: GPP -> CatchClause -> Doc
      ppCatchClause gpp (Catch _2 _4)
	  = fsep [gpp "catch", gpp "(", gpp _2, gpp ")", gpp _4]
	  -- = sep [sep [gpp "catch", gpp "(", gpp _2, gpp ")"], gpp _4]

      ppClassDeclaration :: GPP -> ClassDeclaration -> Doc
      ppClassDeclaration gpp (Class1 _0 _2 _3 _4 _5)
	  = fsep
	      [gppList gpp _0, gpp "class", gpp _2, gppMaybe gpp _3,
	       gppMaybe gpp _4] 
	    $$   
	    (gpp _5)
