module Main where

import CobolLib
import StrategyLib
import IO
import Monad
import System

main = do [fileName] <- getArgs
          prg      <- parseCobolFile fileName
          dotGraph <- toPerformGraph prg
          putStrLn dotGraph

toPerformGraph prg
  = do name   <- getProgramName prg
       labels <- findInvocations prg
       edges  <- findPerformsPerProc labels prg
       main   <- findMain prg
       edges' <- findPerformsInMain main
       return (mkGraph (name++" Perform Graph") (edges++edges'))



-- Simple lookup of names via traversal scheme for selection -----------------

getProgramName :: (MonadPlus m, Term x) => x -> m String
getProgramName
  = applyTU (once_tdTU (failTU `adhocTU` (return . worker)))
    where
      worker (Program_name udw) 
        = user_defined_word2string udw

getSectionName :: Section_with_header -> String
getSectionName (Section_with_header header _)
  = section_header2string header

section_header2string :: Section_header -> String
section_header2string (Section_header sn _)
  = section_name2string sn
  
getParagraphName (Paragraph_1 (Paragraph_11 pname _))
  = paragraph_name2string pname  
getParagraphName (Altered_goto (Altered_goto1 pname _))
  = paragraph_name2string pname  


  
-- Collect performed labels and ranges ---------------------------------------

visitPerform (Perform_statement Nothing _ _ _ _ ) = return []
visitPerform (Perform_statement (Just
              (Perform_statement_procedure p thru_p)) _ _ _ _)
  = return [ ( procedure_name2string p,
               case thru_p of
                 Nothing -> Nothing
                 (Just (Through_procedure_name _ p')) ->
                   Just (procedure_name2string p') ) ]



findInvocations :: (Monad m, Term x) => x -> m [(String, Maybe String)]
findInvocations
  = applyTU (full_tdTU worker)
    where
      worker = constTU [] `adhocTU` visitPerform



-- Create perform edges -------------------------------------------------------

findPerformsPerProc :: (MonadPlus m, Term t)  
                    => [(String, Maybe String)] -> t -> m [DotEdge]

findPerformsPerProc labels
  = applyTU (full_tdTU worker)
    where
      worker = constTU [] `adhocTU` visitParagraph
                          `adhocTU` visitParagraphList
                          `adhocTU` visitSection 
                          `adhocTU` visitSectionList
	     
      visitParagraph (Paragraph_11 pname sentences)
        = do name <- return (paragraph_name2string pname)
             refineProc labels name sentences

      visitParagraphList (paragraphs::[Paragraph])
        = do results <- mapM (refineParagraphs paragraphs) labels
             return (concat results)

      visitSection (Section_with_header header paragraphs)
        = do name   <- return (section_header2string header)
             edges  <- refineProc labels name paragraphs
             return edges

      visitSectionList (sections::[Section_with_header])
        = do results <- mapM (refineSections sections) labels
             return (concat results)

      refineProc labels name proc
        = if (name,Nothing) `elem` labels
           then refineProcBody name proc
           else return []

      refineProcBody name proc
        = do labels <- findInvocations proc
             node   <- return $ mkProc name
             edges  <- return $ map (mkPerf name) labels
             return (node:edges)

      refineParagraphs _ (_, Nothing) = return []
      refineParagraphs paragraphs (startLabel, Just endLabel)
        = let spanned = fromto ((==) startLabel . getParagraphName)
                               ((==) endLabel   . getParagraphName)
                               paragraphs
           in if null spanned
               then return []
               else refineProcBody (mkRangeName startLabel endLabel) spanned

      refineSections _ (_, Nothing) = return []
      refineSections sections (startLabel, Just endLabel)
        = let spanned = fromto ((==) startLabel . getSectionName)
                               ((==) endLabel   . getSectionName)
                               sections
           in if null spanned
               then return []
               else refineProcBody (mkRangeName startLabel endLabel) spanned



-- List pattern matching -----------------------------------------------------

fromto :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
fromto isFrom isTo list
  = let (_,fromAndRest)         = break isFrom list
        (fromUntilTo,toAndRest) = break isTo   fromAndRest
        (to,_)                  = span  isTo   toAndRest
     in fromUntilTo++to



-- Implementation omitted in this distribution -------------------------------

findMain _ = return undefined
findPerformsInMain _ = return []



-- Unparsing helpers ---------------------------------------------------------

procedure_name2string (Procedure_name p_or_s_name (Just (In_section_name _ sn)))
  = (paragraph_or_section_name2string p_or_s_name)
    ++ "IN  "++(section_name2string sn)
procedure_name2string (Procedure_name p_or_s_name Nothing)
  = paragraph_or_section_name2string p_or_s_name

paragraph_or_section_name2string (User_defined_word2 udw)
  = user_defined_word2string udw
paragraph_or_section_name2string (Integer4 i)
 = integer2string i

section_name2string (User_defined_word1 udw)
  = user_defined_word2string udw
section_name2string (Integer3 i)
  = integer2string i

paragraph_name2string (User_defined_word udw)
  = user_defined_word2string udw
paragraph_name2string (Integer2 i)
  = integer2string i

integer2string (Integer5 n)
  = n

user_defined_word2string (User_defined_word3 cw)
  = cobword2string cw

cobword2string (Lex_id str)
  = str



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
  


-- Debugging helpers ---------------------------------------------------------

errLn msg = hPutStrLn stderr msg  
  
------------------------------------------------------------------------------
