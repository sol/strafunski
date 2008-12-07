{-# OPTIONS  -fallow-overlapping-instances #-}
module Main where

import ATermLib

main
  = atermIOwrap "ATermBench" worker
    where
      worker :: ATerm -> IO ATerm
      worker = return
      
instance ATermConvertible ATerm where
  toATerm = id
  fromATerm = id
