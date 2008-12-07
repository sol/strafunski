module Main where

import CobolLib
import IO
import System

main = do [fileName] <- getArgs
          putStrLn $ "Attempting parse of "++fileName
          prg <- parseCobolFile fileName
          putStrLn (toATermString prg)
          return ()
