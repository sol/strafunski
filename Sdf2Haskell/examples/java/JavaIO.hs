module JavaIO where

import ATermLib
import IO
import System
import Java
import JavaATermConvertibleInstances

--- IO wrapper ---------------------------------------------------------------

javaIOwrap :: ([CompilationUnit] -> IO String) -> IO ()
javaIOwrap mtransform
  = do args <- getArgs
       sin  <- mapM readJavaFile args
       tin  <- mapM (return . fromATerm . afunCap . readATerm . dehyphen) sin
       sout <- mtransform tin
       putStrLn sout 
       
split p [] 	= ([],[])
split p (a:as) 
  | p a 	= (a:xs,ys)
  | otherwise	= (xs,a:ys) 
  where (xs,ys) = split p as
  
isFlag ('-':_)	= True
isFlag _        = False

readJavaFile fn
  = do errLn ("Reading file: "++fn)
       readFile fn

--- Auxilliaries -------------------------------------------------------------

errLn = hPutStrLn stderr

------------------------------------------------------------------------------
