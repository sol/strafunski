-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Joost Visser 2004
-- License     :  LGPL
-- 
-- Maintainer  :  joost.visser@di.uminho.pt
-- Stability   :  experimental
-- Portability :  portable
--
-- This module is part of the ATerm library for Haskell. This module
-- provides wrapper functions that take care of IO.
--
-----------------------------------------------------------------------------

module ATermIO where

import System
import ATermAbstractSyntax
import ATermConversion 
import ATermReadWrite
import Char

-----------------------------------------------------------------------------
-- * Transformation wrapper
 
-- | Wrapper function to create a main function in the IO monad, given a
--   program name and a monadic transformation function.
atermIOwrap :: (ATermConvertible t, ATermConvertible a) 
            => ProgramName -> (t -> IO a) -> IO ()
atermIOwrap progName mtransform
  = do args <- getArgs
       opts <- return $ parseOptions progName args
       sin  <- readStream (fin opts)
       tin  <- return . fromATerm . dehyphenAST . readATerm $ sin
       tout <- mtransform $ tin
       sout <- return . toString (format opts) $ tout
       writeStream (fout opts) sout 
    where
      readStream "#stdin#"	= getContents
      readStream f		= readFile f
      writeStream "#stdout#"	= putStr
      writeStream f		= writeFile f
      toString format 
        = case format of 
            "TEXT" -> toATermString
	    "TAF"  -> toSharedATermString
	    _      -> error $ "format unknown: "++"\n"++usage progName    
	
-----------------------------------------------------------------------------
-- * Helpers

type ProgramName = String

-- | Turn hyphens in a String into underscores.
dehyphen :: String -> String
dehyphen str
  = map aux str
    where aux '-' = '_'
          aux c   = c

-- | Turn hyphens in AST into underscores except inside nodes
--   that represent literals.
dehyphenAST :: ATerm -> ATerm
--dehyphenAST t@(AAppl "Sdf_Literal" ts) = t
dehyphenAST (AAppl f ts)    = AAppl (dehyphenUnquoted f) (map dehyphenAST ts)
dehyphenAST (AList ts)      = AList (map dehyphenAST ts)
dehyphenAST t               = t

-- | Turn hyphens in unquoted literal into underscores.
dehyphenUnquoted s@('\"':_) = s
dehyphenUnquoted s = dehyphen s

-- | Turn the first character into upper case.       
headToUpper 	   ::  String -> String
headToUpper []     = []
headToUpper (c:cs) = (toUpper c):cs

-- | Make all AFun's start with an uppercase letter.
afunCap :: ATerm -> ATerm
afunCap (AAppl afun ts) = AAppl (headToUpper afun) (map afunCap ts)
afunCap (AList ts)      = AList (map afunCap ts)
afunCap t               = t

-----------------------------------------------------------------------------
-- * Option handling 

data OptionsATermIO
  = OptionsATermIO { fin :: String, fout :: String, format :: String }

defaultOptionsATermIO :: OptionsATermIO
defaultOptionsATermIO
  = OptionsATermIO { fin = "#stdin#", fout = "#stdout#", format = "TEXT" }
  
parseOptions :: String -> [String] -> OptionsATermIO
parseOptions programName args
  = p args
    where 
      p []			= defaultOptionsATermIO
      p ("-t":args)		= (p args){ format = "TEXT" }
      p ("-s":args)		= (p args){ format = "TAF" }
      p ("-b":args)		= err "BAF format not supported!"
      p ("-i":fname:args)	= (p args){ fin = fname }
      p ("-o":fname:args)	= (p args){ fout = fname }
      p args			= err $ "Can't parse options: "++concat args
      err msg			= error $ msg++"\n"++usage programName

usage :: String -> String
usage programName
  = unlines ["Usage","",
             "  "++programName++" [-i <fname>] [-o <fname>] [-t|-s]",
             "",
	     "Options","",
	     "  -i <fname>    name of input file    (default: stdin)",
	     "  -o <fname>    name of output file   (default: stdout)",
	     "  -t            output format is TEXT (plain text)",
	     "  -s            output format is TAF  (textual sharing)"
	    ]	   
    
-------------------------------------------------------------------------------
