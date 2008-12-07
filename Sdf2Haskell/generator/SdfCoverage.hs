------------------------------------------------------------------------------ 
-- | 
-- Maintainer	: Joost Visser
-- Stability	: experimental
-- Portability	: portable
--
-- This module defines a tool for measuring grammar rule coverage for Sdf
-- grammars.

------------------------------------------------------------------------------

module Main (main) where

import SdfLib
import ATermLib
import SGLR
import System
import Configuration
import Data.Set
import SdfOrdInstances
import Data.Unique (newUnique, hashUnique)
import Data.FiniteMap
import Data.Char
import Debug.Trace
import Text.PrettyPrint.HughesPJ (Mode(..))
import System.Console.GetOpt
import Control.Monad

------------------------------------------------------------------------------
-- * Coverage analysis

-- | The main worker function.
sdfCoverage :: FilePath -> [FilePath] -> String -> FilePath -> IO ()
sdfCoverage tableFile termFiles sortName outputFile = do
  progName <- getProgName
  ps <- fmap filterRejects $ dumpProductions tableFile
  let prodCount = cardinality ps
  errLn $ "["++progName++"] Number of productions: "++(show prodCount)
  bag <- foldM (cummulativeCoverage tableFile sortName ps) (emptyBag) termFiles
  errLn ""
  errLn $ "["++progName++"] --- CUMULATIVE ---"
  reportErrLong ps bag
  let usedProds = mkSet $ keysFM $ fm bag
  let unusedProds = ps `minusSet` usedProds
  reportFile unusedProds outputFile

-- | Obtain the productions from a given parse table as a set of ATerms.
dumpProductions :: FilePath -> IO (Set ATerm)
dumpProductions tableName = do
  unique <- newUnique
  let newName = tableName++(show . hashUnique $ unique)
  let asfixName = newName++".asfix"
  let cmd = "dump-productions "++tableName++" > "++asfixName
  progName <- getProgName
  errLn $ "["++progName++"] "++cmd                      
  exitOK $ system cmd
  asfix <- readFile asfixName
  let ps = map (readATerm . dropWhile isSpace . dropWhile (not . isSpace)) $ lines asfix
  return $ mkSet ps

-- | Remove reject productions from set of productions.
filterRejects :: Set ATerm -> Set ATerm
filterRejects prods
  = mkSet $ filter (not . isReject) $ setToList prods
    where
      isReject (AAppl "prod" [_,_,AAppl "attrs" [AList [AAppl "reject" []]]]) = True
      isReject _ = False

-- | Analyze coverage for a single file, add the result to the cummulative result, 
--   and give an intermediate cummulative report.
cummulativeCoverage :: FilePath -> String -> Set ATerm 
                    -> Bag ATerm -> FilePath -> IO (Bag ATerm)
cummulativeCoverage tableFile sortName prods accu f = do
  b <- coverage1 tableFile f sortName
  let accu' = b `unionBag` accu
  reportErrShort prods accu'
  return accu'
  `catch` \error -> do
    progName <- getProgName
    errLn $ "["++progName++"] Skipping file: "++f
    return accu

-- | Determine coverage for a single term, by parsing it and producing
--   a bag that holds usage counts for each occuring production.  
coverage1 :: FilePath -> FilePath -> String -> IO (Bag ATerm)
coverage1 tableFile termFile sortName = do
  progName <- getProgName
  parseTree <- sglr' tableFile termFile sortName
  if (length $ show parseTree) < 0 
    then errLn $ "["++progName++"] *** NO PARSE *** " 
    else errLn $ "["++progName++"] Parsing completed"
{-
  return $ collectProdBag parseTree
-}
  let result = collectProdBag parseTree
  if result==result
    then return result
    else return $ error "CANNOT HAPPEN"  
  
-- | Collect all productions from a given aterm, and return them in a list.
collectProds :: ATerm -> [ATerm]
collectProds t = worker t []
  where
    worker :: ATerm -> [ATerm] -> [ATerm]
    worker p@(AAppl "prod" ts) = (p:)
    worker (AAppl f ts) = foldr ((.) . worker) id ts
    worker (AList ts) = foldr ((.) . worker) id ts
    worker _ = id
{-
collectProds p@(AAppl "prod" ts) = [p]
collectProds (AAppl f ts) = concatMap collectProds ts
collectProds (AList ts) = concatMap collectProds ts
collectProds _ = []
-}

-- | Collect all productions from a given aterm, and return them in a set.
collectProdSet :: ATerm -> Set ATerm
collectProdSet t = worker t emptySet
  where
    worker :: ATerm -> Set ATerm -> Set ATerm
    worker p@(AAppl "prod" ts) = (`addToSet` p)
    worker (AAppl f ts) = foldr ((.) . worker) id ts
    worker (AList ts) = foldr ((.) . worker) id ts
    worker _ = id

-- | Collect all productions from a given aterm, and return them in a set.
collectProdBag :: ATerm -> Bag ATerm
collectProdBag t = worker t emptyBag
  where
    worker :: ATerm -> Bag ATerm -> Bag ATerm
    worker p@(AAppl "prod" ts) = (`addToBag` p)
--    worker (AAppl f ts) = foldr ((.) . worker) id ts
--    worker (AList ts) = foldr ((.) . worker) id ts
    worker (AAppl f ts) = foldl' (\f t -> f . (worker t)) id ts
    worker (AList ts) = foldl' (\f t -> f . (worker t)) id ts
    worker _ = id

-- | Strict variant of foldl.
foldl' f a []     = a
foldl' f a (x:xs) = (foldl' f $! f a x) xs

------------------------------------------------------------------------------
-- * Reporting results

reportFile :: Set ATerm -> FilePath -> IO ()
reportFile unusedProds outputFile = do  
  progName <- getProgName
  let content = unlines $ map unparseProd $ setToList unusedProds
  writeFile outputFile content
  errLn $ "["++progName++"] Unused productions written to file: "++outputFile
  
reportErrLong :: Set ATerm -> Bag ATerm -> IO ()
reportErrLong prods prodBag = do
  progName <- getProgName
  let prodCount = cardinality prods
  let usedProds = mkSet $ keysFM $ fm prodBag
  let usedProdCount = cardinality usedProds
  let usageCount = sum $ eltsFM $ fm prodBag
  errLn $ "["++progName++"] Number of USED productions: "++(show usedProdCount)
  errLn $ "["++progName++"] Number of production USES : "++(show usageCount)
  let unusedProds = prods `minusSet` usedProds
  let unusedProdCount = cardinality unusedProds
  errLn $ "["++progName++"] Number of productions NOT used: "++(show unusedProdCount)
  errLn $ "["++progName++"] COVERAGE: "++(show (usedProdCount*100 `div` prodCount))++" %"
  let usedVarCount  = cardinality $ mkSet $ map takeSortFromATerm $ setToList usedProds
  let totalVarCount = cardinality $ mkSet $ map takeSortFromATerm $ setToList prods
  errLn $ "["++progName++"] NON-TERMINAL COVERAGE: "++(show (usedVarCount*100 `div` totalVarCount))++ " %"


takeSortFromATerm :: ATerm -> String
takeSortFromATerm = show . getSort . convertProd


reportErrShort :: Set ATerm -> Bag ATerm -> IO ()
reportErrShort prods prodBag = do
  progName <- getProgName
  let prodCount = cardinality prods
  let usedProds = mkSet $ keysFM $ fm prodBag
  let usedProdCount = cardinality usedProds
  errLn $ "["++progName++"] COVERAGE: "++(show (usedProdCount*100 `div` prodCount))++" %"

------------------------------------------------------------------------------
-- * Pretty-printing productions.

-- | Unparse a production represented by an ATerm.
unparseProd :: ATerm -> String
unparseProd = showSdfMode OneLineMode . convertProd

-- | Convert back to AST format a production represented by an ATerm.
convertProd :: ATerm -> Production
convertProd = fromATerm . prefixAppls "Sdf_" . explodeSymbols . dehyphenAST


-- | Helper to deal with irregularities between AsFix versions.
explodeSymbols :: ATerm -> ATerm
explodeSymbols (AAppl "prod" [AList ts, s, as]) 
  = AAppl "prod" [AAppl "list7" [ AList $ map explodeSymbols ts], 
                                  (explodeSymbols s), 
                                  (explodeSymbols as)]
explodeSymbols (AAppl "lit" [AAppl f []]) 
  = AAppl "lit" [AAppl "quoted" [AAppl (show f) []]]
explodeSymbols (AAppl "assoc" [t])
  = AAppl "atr" [t]
explodeSymbols (AAppl "term" [AAppl "cons" [AAppl f []]])
  = AAppl "cons1" [AAppl "fun" [AAppl "Literal" [AAppl "quoted" [AAppl (show f) []]]]]
explodeSymbols (AAppl "char_class" [AList [r]])
  = AAppl "char_class1" [
      AAppl "simple_charclass" [
        AAppl "present1" [explodeCharRange r]]]
explodeSymbols (AAppl "char_class" [AList rs])
  = AAppl "char_class1" [
      AAppl "simple_charclass" [
        AAppl "present1" 
          [foldr1 (\r1 r2 -> AAppl "conc" [r1,r2]) (map explodeCharRange rs)]]]
explodeSymbols (AAppl "seq" [AList (t:ts)]) 
  = AAppl "seq1" [
      explodeSymbols t,
      AList (map explodeSymbols ts) ]
explodeSymbols (AAppl "alt" ts)
  = AAppl "alt1" (map explodeSymbols ts)
explodeSymbols (AAppl "iter_sep_n" [t1,t2,AInt n])
  = AAppl "iter_sep_n" [
      explodeSymbols t1,
      explodeSymbols t2,
      AAppl (show $ show n) [] ]
explodeSymbols (AAppl f ts) = AAppl f (map explodeSymbols ts)
explodeSymbols (AList ts) = AList (map explodeSymbols ts)
explodeSymbols p = p

mytrace t
  = trace ("TRACE: "++(show t)) t

-- | Helper to deal with irregularities between AsFix versions.
explodeCharRange (AInt i)
  = AAppl "CharRange" [
      AAppl "Character" [AAppl "numeric" [AAppl (quote $ show i) []]]]
explodeCharRange (AAppl "range" [AInt i,AInt j])
  = AAppl "CharRange" [
      AAppl "range" [AAppl "numeric" [AAppl (quote $ show i) []],
                     AAppl "numeric" [AAppl (quote $ show j) []]]]

-- | Helper to deal with discrepancy between constructor attibutes in
--   our grammar of Sdf, and the one underlying the standard Sdf tools.
prefixAppls :: String -> ATerm -> ATerm
prefixAppls pre = worker
  where
    worker (AAppl f@('"':_) ts) = AAppl f ts
    worker (AAppl f ts) = AAppl (pre++f) (map worker ts)
    worker (AList ts)   = AList (map worker ts)
    worker t = t

-- | Perhaps better use show?
quote :: String -> String
quote s
  = "\""++(concatMap esc s)++"\""
    where
      esc '"' = ['\\','"']
      esc c = [c]

------------------------------------------------------------------------------
-- * Parsing.

-- | Call sglr to parse a file, and return the resulting Asfix parse tree as
--   an ATerm. The difference with SGLR.sglr is that no implosion is done.
sglr' :: FilePath -> FilePath -> String -> IO ATerm
sglr' tableName termName sortName
  = do unique <- newUnique
       let newName = termName++(show . hashUnique $ unique)
       let bafName = newName++".asfix.baf"
       let tafName = newName++".asfix.taf"
       let sglrCmd = "sglr -p "++tableName++
                         " -i "++termName++
                         " -o "++bafName++
       --                   " -s "++sortName++
                         " -2"
       let baffleCmd = "baf2taf < "++bafName++
                              " > "++tafName
       progName <- getProgName
       errLn $ "["++progName++"] "++sglrCmd                      
       exitOK $ system sglrCmd
       errLn $ "["++progName++"] "++baffleCmd                      
       exitOK $ system baffleCmd
       asfix <- readFile tafName
       let aterm = readATerm $ asfix
       return aterm

------------------------------------------------------------------------------
-- * Command line

-- | Main program.
main :: IO ()
main = do
  args <- getArgs
  (flags,trailingArgs) <- cmdLine args
  let reqArgs = reqArgsFromFlags flags
  case reqArgs of
    (Nothing)      -> putStr cmdUsageInfo 
    (Just (t,s,r)) -> sdfCoverage t trailingArgs s r

-- | Command line flags.
data Flag 
  = ParseTable String 
  | StartSymbol String
  | ReportFile String
  | Help
  deriving (Eq, Show)
  
-- | Option descriptions for command line flags.
options :: [OptDescr Flag]
options = 
	[ Option ['p'] ["table"] (ReqArg ParseTable "Parse Table") "Parse Table"
	, Option ['s'] ["symbol"] (ReqArg StartSymbol "Start Symbol") "Start Symbol"
	, Option ['o'] ["report"] (ReqArg ReportFile "Report") "Report"
	, Option ['h','?'] ["help"] (NoArg Help) "Show this help screen"
	]

-- | Process the command line arguments into flags and remaining arguments.
cmdLine :: [String] -> IO ([Flag], [String])
cmdLine argv = 
	case (getOpt RequireOrder options argv) of
		(o, n, []) -> return (o,n)
		(_,_,errs) -> cmdLineError errs

-- | Report command line error.
cmdLineError :: [String] -> IO a
cmdLineError errs = ioError $ userError $ concat errs ++ cmdUsageInfo

-- | Produce usage information.
cmdUsageInfo :: String
cmdUsageInfo = usageInfo header options
	where header = "Usage: "

-- | Extract the required arguments from the list of command line flags.
reqArgsFromFlags :: [Flag] -> Maybe (FilePath,String,FilePath)
reqArgsFromFlags flags
  = case (justFilter unT flags,justFilter unS flags,justFilter unR flags) of
      ([t],[s],[r]) 
         -> Just (t,s,r)
      _  -> Nothing
    where
      unT (ParseTable s)  = Just s
      unT _               = Nothing
      unS (StartSymbol s) = Just s
      unS _               = Nothing
      unR (ReportFile s)  = Just s
      unR _               = Nothing

-- | Utility function.
justFilter :: (a -> Maybe b) -> [a] -> [b]
justFilter f [] = []
justFilter f (a:as) 
  = case (f a) of
      Nothing    -> justFilter f as
      (Just b)   -> (b:justFilter f as) 

------------------------------------------------------------------------------
-- * Tests

test :: IO ()
test =
  sdfCoverage 
    "../examples/littlelambda/LittleLambda.tbl"
    ["../examples/littlelambda/ex1.expr",
     "../examples/littlelambda/ex2.expr",
     "../examples/littlelambda/doesnotexist",
     "../examples/littlelambda/Makefile"]
    "Expr"
    "coverage-report"
    
testJava :: IO ()
testJava =
  sdfCoverage 
    "../examples/java/Java.tbl"
    ["../examples/java/Hoi.java",
     "../examples/java/Hello.java",
     "../examples/java/Verbatim.java"]
    "CompilationUnit"
    "coverage-report"

testVDM :: IO ()
testVDM =
  sdfCoverage
    "/Users/joost/Research/VooDooMFront/isovdm-front-0.0.1/syn/isovdm.tbl"
    []
    "Document"
    "coverage-report"

------------------------------------------------------------------------------
-- * Bags

-- | The type of bags.
newtype Bag a = Bag {fm :: FiniteMap a Int} deriving Eq

-- | Add an element to a bag.
addToBag :: Ord a => Bag a -> a -> Bag a
addToBag Bag{ fm=bag } a
  = Bag (addToFM_C (+) bag a 1)

-- | Empty bag.
emptyBag :: Bag a
emptyBag = Bag emptyFM

-- | Put all elements from a list into a bag.
listToBag :: Ord a => [a] -> Bag a
listToBag xs
  = foldr (\x bag -> addToBag bag x) emptyBag xs

-- | Create the union of a list of bags.
unionBags :: Ord a => [Bag a] -> Bag a
unionBags bags = 
  Bag $ foldr (plusFM_C (+)) emptyFM $ map fm bags
  
-- | Create the union of two bags.
unionBag :: Ord a => Bag a -> Bag a -> Bag a
unionBag b1 b2 = 
  Bag $ plusFM_C (+) (fm b1) (fm b2)

------------------------------------------------------------------------------
