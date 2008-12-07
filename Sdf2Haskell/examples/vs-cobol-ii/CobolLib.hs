module CobolLib (

 parseCobol, parseCobolFile,
 module CobolSyntax,
 module CobolSyntaxTermInstances,
 module CobolSyntaxATermConvertibleInstances,
 module ATermLib

) where

import CobolSyntax
import CobolSyntaxTermInstances
import CobolSyntaxATermConvertibleInstances
import ATermLib
import SGLR
import Configuration



-- We just read in an ATerm; parsing is done externally ----------------------

parseCobol :: IO Cobol_source_program
 = do inp <- getContents
      return (fromATerm . afunCap . readATerm $ (dehyphen inp))



-- We parse from within Haskell

parseCobolFile :: FilePath -> IO Cobol_source_program
parseCobolFile fileName
  = do let tableName = compilationDir++"/CobolSyntax.tbl"
       let sortName = "Cobol-source-program"
       --putStrLn $ "sglr -p "++" -i "++fileName++" -s "++sortName
       sglr tableName fileName sortName

--------------------
