------------------------------------------------------------------------------ 
-- | 
-- Maintainer	: Ralf Laemmel, Joost Visser
-- Stability	: experimental
-- Portability	: portable
--
-- This module is part of 'Sdf2Haskell', a tool for generating a set of
-- Haskell data types from an SDF grammar. This module provides functionality
-- for computing some metrics over SDF grammars.

------------------------------------------------------------------------------

module SdfMetrics where

import Sdf
import StrategyLib hiding (putMetricLn)
import System.IO

--- SDF Metrics -------------------------------------------------------------

-- | Extract metrics from an SDF grammar.  
sdfMetrics :: SDF -> Metrics
sdfMetrics sdf
  = maybe initMetrics0 id (applyTU (full_tdTU atnode) sdf)
    where
      atnode = failTU 
               `typeMetric` ("modules    ",  typeGuard :: TypeGuard Module)
               `typeMetric` ("productions",  typeGuard :: TypeGuard Production)
               `typeMetric` ("symbols    ",  typeGuard :: TypeGuard Symbol)
               `predMetric` ("sorts      ",  isSort)
      isSort (Sdf_sort _) = Just ()
      isSort _            = Nothing

-- | Extract metrics from an SDF grammar, and print them to standard output.
putSdfMetricsLns :: SDF -> IO ()
putSdfMetricsLns sdf
  = do metrics <- return . sdfMetrics $ sdf
       putMetricLn "modules    " metrics
       putMetricLn "sorts      " metrics
       putMetricLn "productions" metrics
       putMetricLn "symbols    " metrics    

-- | Print value of metric with the given name.
putMetricLn		:: MetricName -> Metrics -> IO ()
putMetricLn key m 	=  hPutStrLn stderr $ key++" = "++show (m key)
 

-------------------------------------------------------------------------------
