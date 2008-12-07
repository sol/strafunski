module JavaChaseImports where

import Java
import JavaATermConvertibleInstances
import JavaTermInstances
import JavaUtil
import ChaseImports
import StrategyLib
import ATermLib
import System
import Control.Monad.Identity
import Configuration
import SGLR

main'
  = do args <- getArgs
       cus <- javaChaseImports ["."] args
       putStrLn ("Found "++(show (length cus))++" compilation units")

-- Java import chasing is implemented by instantiation of our
-- generic import chasing function.

javaChaseImports clspth clsnames
  = chaseWith clspth clsnames [] [] parse get_imports onModule onMissing
    where
      parse clspth clsname      = parseJavaFileOnPath clspth clsname
      get_imports cu		= javaGetImports cu
      onModule mn is m cus	= return (m:cus)
      onMissing	mn cus		= return cus

-- Extract all imported class names from a compilation unit

javaGetImports :: CompilationUnit -> [ChaseName]
javaGetImports (CU pkg_decl imports types)
  = map qname2str used_cls_names
    where
      used_cls_names 
        = runIdentity (applyTU (full_tdTU worker) (imports,types))

      worker   = constTU [] `adhocTU` getClsOrIfcType
                            `adhocTU` getArrType
                            `adhocTU` getImportedType

      getClsOrIfcType (Name name)             = return [name]
      getArrType (ArrayType0 name)            = return [name]
      getImportedType (Import_semicolon name) = return [name]

-- Find and read a pre-parsed java file
 
parseJavaFileOnPath :: [FilePath] -> ChaseName 
                    -> IO (Either CompilationUnit String)
parseJavaFileOnPath clspth clsname
  = do --content <- chaseFile clspth relativeName ["java.af"]
       --let cu = fromATerm . afunCap . readATerm . dehyphen $ content
       --return (Left cu)   
       fileName <- findFile clspth relativeName ["java"]
       cu <- sglr tableName fileName "CompilationUnit"
       return (Left cu)
    where
      relativeName = map dot2slash clsname
      dot2slash '.' = '/'
      dot2slash c   = c
      tableName = compilationDir++"/Java.tbl"

----------------------------------------------------------------------------
