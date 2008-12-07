module Main where

import Java
import JavaATermConvertibleInstances
import JavaTermInstances
import JavaChaseImports
import ChaseImports
import StrategyLib
import ATermLib
import System
import DTDJavaMetrics
import DTDJavaMetricsTermInstances
import Text.XML.HaXml.Xml2Haskell
import IO
import Monad



--- For processing a single compilation unit per file ------------------------

main 
  = do errLn "\n == Computation of metrics for Java == \n"
       args <- getArgs
       cus <- javaChaseImports ["."] args
       errLn ""
       errLn ("  Found "++(show (length cus))++" compilation units")
       stat_num <- statementCounter cus
       errLn ("  Counted "++(show stat_num)++" non-empty statements")
       nest_dept <- nestingDepth cus
       errLn ("  Measured "++(show nest_dept)++" nesting depth")
       mccabe <- mcCabeIndex cus
       errLn ("  Measured McCabe index of: "++(show mccabe))
       metrics <- extractJavaMetrics cus
       errLn ("  Exporting computed metrics to XML.")
       writeXml metrics
       errLn "\n == Done. == \n"

writeXml = fWriteXml "-"


--- Metrics for Java statement count -----------------------------------------

statementCounter :: (Term t, Monad m) => t -> m Int
statementCounter cus
  = applyTU (full_tdTU worker) cus
    where 
      worker
        = constTU 0 `adhocTU` stat
                  `adhocTU` localVarDecStat

      stat s = case s of
                 (StatementWithoutTrailingSubstatement (EmptyStatement _)) 
                                    -> return 0
                 (StatementWithoutTrailingSubstatement (Block _)) 
                                    -> return 0
                 (LabeledStatement _)
                                    -> return 0
                 (ClassDeclaration _) 
                                    -> return 0
                 _                  -> return 1

      localVarDecStat (_::LocalVariableDeclarationStatement) = return 1



-- Metrics for Java nesting depth of conditionals ----------------------------

nestingDepth :: (Term t, MonadPlus m) => t -> m Int
nestingDepth cus
  = applyTU (depthWith worker) cus
    where
      worker = isConditional



--- Reusable recogniser for Java conditionals --------------------------------

isConditional :: MonadPlus m => TU () m
isConditional 
  = failTU
            `adhocTU` (\(_::IfThenStatement)     -> return ())
            `adhocTU` (\(_::IfThenElseStatement) -> return ())
            `adhocTU` (\(_::WhileStatement)      -> return ())
            `adhocTU` (\(_::ForStatement)        -> return ())
            `adhocTU` (return.(typeGuard::TypeGuard IfThenElseStatementNoShortIf))
            `adhocTU` (return.(typeGuard::TypeGuard WhileStatementNoShortIf))
            `adhocTU` (return.(typeGuard::TypeGuard ForStatementNoShortIf))
            `adhocTU` (return.(typeGuard::TypeGuard TryStatement))

          -- maybe we prefer the following style (simpler)

            `adhocTU` (\(_::TryStatement) -> return ())



--- Java McCabe index --------------------------------------------------------

mcCabeIndex :: (Term t, MonadPlus m) => t -> m Int
mcCabeIndex cus
  = applyTU (full_tdTU worker) cus
    where
      worker = tryTU (isConditional `passTU` \() -> constTU 1)



--- Java metrics extraction --------------------------------------------------

extractJavaMetrics cus
  = do cuMetrics <- mapM extractCuMetrics cus
       return $ JavaMetrics cuMetrics 

extractCuMetrics (CU _ _ types)
  = do let classes   =  getClasses types
       classMetrics  <- mapM extractClassMetrics classes
       return $
         CuMetric
           CuMetric_Attrs { cuMetricName = str2cdata "NoName" }
           (map CuMetric_ClassMetric classMetrics)
    where
      getClasses types = map unClsTpDecl (filter isClsTpDecl types)
      isClsTpDecl (ClassTypeDeclaration cd) = True
      isClsTpDecl _                         = False
      unClsTpDecl (ClassTypeDeclaration cd) = cd
      unClsTpDecl _ = error "holala"

extractClassMetrics :: MonadPlus m => ClassDeclaration -> m ClassMetric
extractClassMetrics (Class1 _ name extends implements body)
  = do let fields         =  getFields body
       let fieldCount     =  length fields
       let nestedClasses  =  getNestedClasses body
       nestedClassMetrics <- mapM extractClassMetrics nestedClasses
       let methods        =  getMethods body
       methodMetrics      <- mapM extractMethodMetrics methods
       return $
         ClassMetric
           ClassMetric_Attrs {
             classMetricName       = str2cdata name,
             classMetricFieldCount = int2cdata fieldCount } 
           ( (map ClassMetric_ClassMetric nestedClassMetrics)
             ++ 
             (map ClassMetric_MethodMetric methodMetrics) )
    where
      getFields (ClassBodyDeclaration_s decls)
        = map unField (filter isField decls)
      isField (ClassMemberDeclaration (FieldDeclaration1 fd)) = True 
      isField _                                               = False
      unField (ClassMemberDeclaration (FieldDeclaration1 fd)) = fd 
      unField _ = error "Hoepla"

      getNestedClasses (ClassBodyDeclaration_s decls)
        = map unClassDecl (filter isClassDecl decls)
      isClassDecl (ClassDeclaration1 cd) = True
      isClassDecl _                      = False
      unClassDecl (ClassDeclaration1 cd) = cd
      unClassDecl _ = error "Hola"

      getMethods (ClassBodyDeclaration_s decls)
        = map unMethod (filter isMethod decls)
      isMethod (ClassMemberDeclaration (MethodDeclaration md)) = True 
      isMethod _                                               = False
      unMethod (ClassMemberDeclaration (MethodDeclaration md)) = md 
      unMethod _ = error "Hela"


extractMethodMetrics :: MonadPlus m
                     => MethodDeclaration -> m MethodMetric
extractMethodMetrics (MethodHeader_MethodBody header body)
  = do name               <- getMethodName header
       statCount          <- statementCounter body
       mcCabe             <- mcCabeIndex body
       nestingDepth       <- nestingDepth body
       nestedClasses      <- getNestedClasses body
       nestedClassMetrics <- mapM extractClassMetrics nestedClasses
       return $
         MethodMetric 
           MethodMetric_Attrs {
             methodMetricName           = str2cdata name,
             methodMetricStatementCount = int2cdata statCount,
             methodMetricMcCabe         = int2cdata mcCabe,
             methodMetricNestingDepth   = int2cdata nestingDepth }
           nestedClassMetrics
    where
      getNestedClasses body
        = applyTU (stop_tdTU getClassDecl) body
      getClassDecl
        = failTU `adhocTU` (\(cd::ClassDeclaration) -> return [cd])



--- More HTMLish API ---------------------------------------------------------

type CDATA = String

str2cdata :: String -> CDATA
str2cdata s = s

int2cdata :: Int -> CDATA
int2cdata i = show i 

    

--- Extract method name from its header --------------------------------------

getMethodName header
  = case (applyTU (once_buTU (failTU `adhocTU` worker)) header) of
      Just ident  -> return ident
      Nothing     -> error "Method without a name?"
    where
      worker (Comma2 ident _) = return ident
      worker _                = mzero



--- Visualize XML ------------------------------------------------------------
--- We show only nodes for classes and methods, not for the entire -----------
--- system or for compilation units. -----------------------------------------

visualizeJavaMetrics :: MonadPlus m => JavaMetrics -> m [DotElement]
visualizeJavaMetrics m
  = visualizeMetrics ""  m

visualizeMetrics :: (Term t, MonadPlus m) => String -> t -> m [DotElement]
visualizeMetrics parent m
  = applyTU (stop_tdTU worker) m
    where 
      worker = failTU `adhocTU` method
                      `adhocTU` clss

      method (MethodMetric attrs nestedMetrics)
        = do let label     = cdata2str (methodMetricName attrs)
             let statcount = methodMetricStatementCount attrs
             let node      = (dotNode label) { height = cdata2int statcount }
             let edge      = dotEdge parent label
             dotElements <- visualizeMetrics label nestedMetrics
             return (node:edge:dotElements)

      clss (ClassMetric attrs nestedMetrics)
        = do let label      = cdata2str (classMetricName attrs)
             let fieldCount = cdata2int (classMetricFieldCount attrs)
             let otherCount = length nestedMetrics
             let node       = (dotNode label) { height = fieldCount+otherCount }
             let edge       = dotEdge parent label
             dotElements <- visualizeMetrics label nestedMetrics
             return (node:edge:dotElements)



--- dot API ------------------------------------------------------------------

data DotElement 
  = DotNode { 
      label  :: String, 
      shape  :: String, 
      height :: Int }
  | DotEdge {
      source :: String,
      target :: String }

dotNode label
  = DotNode label "box" 1

dotEdge source target
  = DotEdge source target



--- More API -----------------------------------------------------------------

cdata2int :: CDATA -> Int
cdata2int str = read str

cdata2str :: CDATA -> String
cdata2str = id



-- dot helpers ---------------------------------------------------------------

type DotGraph = String
type DotEdge  = String

mkGraph name edges   = "digraph "++(quote name)++" {\n"++(concat edges)++"}\n"
mkNest n1 n2         = (quote n1)++" -> "++(quote n2)++"\n"
mkProc n             = (quote n)++" [ shape=box ]\n"
mkPerf n (p,Nothing) = (quote n)++" -> "++(quote p)++"\n"
mkPerf n (p,Just q)  = (quote n)++" -> "++(quote (mkRangeName p q))++"\n"
mkRangeName p q      = p++".."++q

quote n = "\""++n++"\""



--- Progress and error messages ----------------------------------------------

errLn str = hPutStrLn stderr str



----------------------------------------------------------------------------
