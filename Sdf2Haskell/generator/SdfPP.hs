module SdfPP where
import Sdf
import Text.PrettyPrint.HughesPJ
import GPP
import StrategyLib
 
instance PP Grammar where
	pp gpp (Sdf_aliases _1) = fsep [gpp "aliases", gpp _1]
	pp gpp (Sdf_restrictions _1) = fsep [gpp "restrictions", gpp _1]
	pp gpp (Sdf_sorts_ _1) = fsep [gpp "sorts", gpp _1]
	pp gpp (Sdf_priorities _1) = fsep [gpp "priorities", gpp _1]
	pp gpp (Sdf_imp_section _0) = fsep [gpp _0]
	pp gpp (Sdf_lexical_syntax _2)
	  = fsep [gpp "lexical", gpp "syntax", gpp _2]
	pp gpp (Sdf_context_free_syntax _2)
	  = fsep [gpp "context-free", gpp "syntax", gpp _2]
	pp gpp (Sdf_variables _1) = fsep [gpp "variables", gpp _1]
	pp gpp (Sdf_lexical_variables _2)
	  = fsep [gpp "lexical", gpp "variables", gpp _2]
	pp gpp (Sdf_empty_grammar) = fsep [gpp "(/)"]
	pp gpp (Sdf_conc_grammars _0 _1) = fsep [gpp _0, gpp _1]
	pp gpp (Sdf_syntax _1) = fsep [gpp "syntax", gpp _1]
	pp gpp (Sdf_lexical_priorities _2)
	  = fsep [gpp "lexical", gpp "priorities", gpp _2]
	pp gpp (Sdf_context_free_priorities _2)
	  = fsep [gpp "context-free", gpp "priorities", gpp _2]
	pp gpp (Sdf_lexical_restrictions _2)
	  = fsep [gpp "lexical", gpp "restrictions", gpp _2]
	pp gpp (Sdf_context_free_restrictions _2)
	  = fsep [gpp "context-free", gpp "restrictions", gpp _2]
 
instance PP Alias where
	pp gpp (Sdf_alias _0 _2) = fsep [gpp _0, gpp "->", gpp _2]
 
instance PP Aliases where
	pp gpp (Sdf_list _0) = fsep [gppList gpp _0]
 
instance PP Lookahead where
	pp gpp (Sdf_char_class _0) = fsep [gpp _0]
	pp gpp (Sdf_seq _0 _2) = fsep [gpp _0, gpp ".", gpp _2]
 
instance PP Lookaheads where
	pp gpp (Sdf_single _0) = fsep [gpp _0]
	pp gpp (Sdf_alt _0 _2) = fsep [gpp _0, gpp "|", gpp _2]
	pp gpp (Sdf_list1 _1)
	  = fsep [gpp "[[", gppListSep gpp "," _1, gpp "]]"]
 
instance PP Restriction where
	pp gpp (Sdf_follow _0 _2) = fsep [gpp _0, gpp "-/-", gpp _2]
 
instance PP Restrictions where
	pp gpp (Sdf_list2 _0) = fsep [gppList gpp _0]
 
instance PP Attribute where
	pp gpp (Sdf_reject) = fsep [gpp "reject"]
	pp gpp (Sdf_prefer) = fsep [gpp "prefer"]
	pp gpp (Sdf_avoid) = fsep [gpp "avoid"]
	pp gpp (Sdf_cons1 _2) = fsep [gpp "cons", gpp "(", gpp _2, gpp ")"]
	pp gpp (Sdf_constructor) = fsep [gpp "constructor"]
	pp gpp (Sdf_memo) = fsep [gpp "memo"]
	pp gpp (Sdf_traverse) = fsep [gpp "traverse"]
	pp gpp (Sdf_bracket) = fsep [gpp "bracket"]
	pp gpp (Sdf_atr _0) = fsep [gpp _0]
	pp gpp (Sdf_id _2) = fsep [gpp "id", gpp "(", gpp _2, gpp ")"]
 
instance PP OptExp where
	pp gpp (Sdf_present _1) = fsep [gpp "e", gpp _1]
	pp gpp (Sdf_absent) = fsep []
 
instance PP RealCon where
	pp gpp (Sdf_real_con _0 _2 _3)
	  = fsep [gpp _0, gpp ".", gpp _2, gpp _3]
 
instance PP AFun where
	pp gpp (Sdf_Literal _0) = fsep [gpp _0]
 
instance PP ATerm' where
	pp gpp (Sdf_fun _0) = fsep [gpp _0]
 
instance PP Symbol where
	pp gpp (Sdf_label _0 _2) = fsep [gpp _0, gpp ":", gpp _2]
	pp gpp (Sdf_lit _0) = fsep [gpp _0]
	pp gpp (Sdf_sort _0) = fsep [gpp _0]
	pp gpp (Sdf_char_class1 _0) = fsep [gpp _0]
	pp gpp (Sdf_empty1) = fsep [gpp "(", gpp ")"]
	pp gpp (Sdf_seq1 _1 _2)
	  = fsep [gpp "(", gpp _1, gppList gpp _2, gpp ")"]
	pp gpp (Sdf_opt _0) = fsep [gpp _0, gpp "?"]
	pp gpp (Sdf_iter _0) = fsep [gpp _0, gpp "+"]
	pp gpp (Sdf_iter_star _0) = fsep [gpp _0, gpp "*"]
	pp gpp (Sdf_iter_sep _1 _2)
	  = fsep [gpp "{", gpp _1, gpp _2, gpp "}", gpp "+"]
	pp gpp (Sdf_iter_star_sep _1 _2)
	  = fsep [gpp "{", gpp _1, gpp _2, gpp "}", gpp "*"]
	pp gpp (Sdf_iter_n _0 _1) = fsep [gpp _0, gpp _1, gpp "+"]
	pp gpp (Sdf_iter_sep_n _1 _2 _4)
	  = fsep [gpp "{", gpp _1, gpp _2, gpp "}", gpp _4, gpp "+"]
	pp gpp (Sdf_set _2) = fsep [gpp "Set", gpp "[", gpp _2, gpp "]"]
	pp gpp (Sdf_pair _0 _2) = fsep [gpp _0, gpp "#", gpp _2]
	pp gpp (Sdf_func _1 _3)
	  = fsep [gpp "(", gpp _1, gpp "=>", gpp _3, gpp ")"]
	pp gpp (Sdf_alt1 _0 _2) = fsep [gpp _0, gpp "|", gpp _2]
	pp gpp (Sdf_perm _1) = fsep [gpp "<<", gpp _1, gpp ">>"]
	pp gpp (Sdf_cf _1) = fsep [gpp "<", gpp _1, gpp "-CF", gpp ">"]
	pp gpp (Sdf_lex _1) = fsep [gpp "<", gpp _1, gpp "-LEX", gpp ">"]
	pp gpp (Sdf_varsym _1)
	  = fsep [gpp "<", gpp _1, gpp "-VAR", gpp ">"]
	pp gpp (Sdf_layout) = fsep [gpp "LAYOUT"]
	pp gpp (Sdf_start) = fsep [gpp "<START>"]
	pp gpp (Sdf_file_start) = fsep [gpp "<Start>"]
 
instance PP Literal where
	pp gpp (Sdf_quoted _0) = fsep [gpp _0]
	pp gpp (Sdf_uqlit _0) = fsep [gpp _0]
 
instance PP Production where
	pp gpp (Sdf_prod_fun _0 _2 _5 _6)
	  = fsep
	      [gpp _0, gpp "(", gppListSep gpp "," _2, gpp ")", gpp "->", gpp _5,
	       gpp _6]
	pp gpp (Sdf_prod _0 _2 _3)
	  = fsep [gpp _0, gpp "->", gpp _2, gpp _3]
 
instance PP Character where
	pp gpp (Sdf_numeric _0) = fsep [gpp _0]
	pp gpp (Sdf_short _0) = fsep [gpp _0]
	pp gpp (Sdf_top) = fsep [gpp "\\\\TOP"]
	pp gpp (Sdf_eof) = fsep [gpp "\\\\EOF"]
	pp gpp (Sdf_bot) = fsep [gpp "\\\\BOT"]
	pp gpp (Sdf_label_start) = fsep [gpp "\\\\LABEL_START"]
 
instance PP CharRange where
	pp gpp (Sdf_Character _0) = fsep [gpp _0]
	pp gpp (Sdf_range _0 _2) = fsep [gpp _0, gpp "-", gpp _2]
 
instance PP CharRanges where
	pp gpp (Sdf_CharRange _0) = fsep [gpp _0]
	pp gpp (Sdf_conc _0 _1) = fsep [gpp _0, gpp _1]
 
instance PP OptCharRanges where
	pp gpp (Sdf_absent1) = fsep []
	pp gpp (Sdf_present1 _0) = fsep [gpp _0]
 
instance PP CharClass where
	pp gpp (Sdf_simple_charclass _1) = fsep [gpp "[", gpp _1, gpp "]"]
	pp gpp (Sdf_comp _1) = fsep [gpp "~", gpp _1]
	pp gpp (Sdf_diff _0 _2) = fsep [gpp _0, gpp "/", gpp _2]
	pp gpp (Sdf_isect _0 _2) = fsep [gpp _0, gpp "/\\\\", gpp _2]
	pp gpp (Sdf_union _0 _2) = fsep [gpp _0, gpp "\\\\/", gpp _2]
 
instance PP Associativity where
	pp gpp (Sdf_left) = fsep [gpp "left"]
	pp gpp (Sdf_right) = fsep [gpp "right"]
	pp gpp (Sdf_non_assoc) = fsep [gpp "non-assoc"]
	pp gpp (Sdf_assoc) = fsep [gpp "assoc"]
 
instance PP Group where
	pp gpp (Sdf_simple_group _0) = fsep [gpp _0]
	pp gpp (Sdf_prods_group _1) = fsep [gpp "{", gpp _1, gpp "}"]
	pp gpp (Sdf_assoc_group _1 _3)
	  = fsep [gpp "{", gpp _1, gpp ":", gpp _3, gpp "}"]
 
instance PP Priority where
	pp gpp (Sdf_chain _0) = fsep [gppListSep gpp ">" _0]
	pp gpp (Sdf_assoc1 _0 _1 _2) = fsep [gpp _0, gpp _1, gpp _2]
 
instance PP Priorities where
	pp gpp (Sdf_comma _0) = fsep [gppListSep gpp "," _0]
 
instance PP IntCon where
	pp gpp (Sdf_natural _0) = fsep [gpp _0]
	pp gpp (Sdf_positive _1) = fsep [gpp "+", gpp _1]
	pp gpp (Sdf_negative _1) = fsep [gpp "-", gpp _1]
 
instance PP Renamings where
	pp gpp (Sdf_renamings _1) = fsep [gpp "[", gppList gpp _1, gpp "]"]
 
instance PP Renaming where
	pp gpp (Sdf_symbol _0 _2) = fsep [gpp _0, gpp "=>", gpp _2]
	pp gpp (Sdf_production _0 _2) = fsep [gpp _0, gpp "=>", gpp _2]
 
instance PP Definition where
	pp gpp (Sdf_list4 _0) = fsep [gppList gpp _0]
 
instance PP Module where
	pp gpp (Sdf_module_ _1 _2 _3)
	  = fsep [gpp "module", gpp _1, gppList gpp _2, gpp _3]
 
instance PP Section where
	pp gpp (Sdf_exports_ _1) = fsep [gpp "exports", gpp _1]
	pp gpp (Sdf_hiddens _1) = fsep [gpp "hiddens", gpp _1]
 
instance PP Sections where
	pp gpp (Sdf_list5 _0) = fsep [gppList gpp _0]
 
instance PP ModuleName where
	pp gpp (Sdf_unparameterized _0) = fsep [gpp _0]
	pp gpp (Sdf_parameterized _0 _2)
	  = fsep [gpp _0, gpp "[", gpp _2, gpp "]"]
 
instance PP ImpSection where
	pp gpp (Sdf_imports_ _1) = fsep [gpp "imports", gpp _1]
 
instance PP Imports where
	pp gpp (Sdf_list6 _0) = fsep [gppList gpp _0]
 
instance PP Import where
	pp gpp (Sdf_module1 _0) = fsep [gpp _0]
	pp gpp (Sdf_renamed_module _0 _1) = fsep [gpp _0, gpp _1]
 
instance PP Symbols where
	pp gpp (Sdf_list7 _0) = fsep [gppList gpp _0]
 
instance PP Attributes where
	pp gpp (Sdf_attrs _1)
	  = fsep [gpp "{", gppListSep gpp "," _1, gpp "}"]
	pp gpp (Sdf_no_attrs) = fsep []
 
instance PP Productions where
	pp gpp (Sdf_list8 _0) = fsep [gppList gpp _0]
 
instance PP SDF where
	pp gpp (Sdf_definition _1) = fsep [gpp "definition", gpp _1]
 
uppSdf :: UPP
uppSdf gpp
  = (constTU empty) `adhocQ` (pp gpp :: MonoPP String) `adhocQ`
      (pp gpp :: MonoPP Grammar)
      `adhocQ` (pp gpp :: MonoPP Alias)
      `adhocQ` (pp gpp :: MonoPP Aliases)
      `adhocQ` (pp gpp :: MonoPP Lookahead)
      `adhocQ` (pp gpp :: MonoPP Lookaheads)
      `adhocQ` (pp gpp :: MonoPP Restriction)
      `adhocQ` (pp gpp :: MonoPP Restrictions)
      `adhocQ` (pp gpp :: MonoPP Attribute)
      `adhocQ` (pp gpp :: MonoPP OptExp)
      `adhocQ` (pp gpp :: MonoPP RealCon)
      `adhocQ` (pp gpp :: MonoPP AFun)
      `adhocQ` (pp gpp :: MonoPP ATerm')
      `adhocQ` (pp gpp :: MonoPP Symbol)
      `adhocQ` (pp gpp :: MonoPP Literal)
      `adhocQ` (pp gpp :: MonoPP Production)
      `adhocQ` (pp gpp :: MonoPP Character)
      `adhocQ` (pp gpp :: MonoPP CharRange)
      `adhocQ` (pp gpp :: MonoPP CharRanges)
      `adhocQ` (pp gpp :: MonoPP OptCharRanges)
      `adhocQ` (pp gpp :: MonoPP CharClass)
      `adhocQ` (pp gpp :: MonoPP Associativity)
      `adhocQ` (pp gpp :: MonoPP Group)
      `adhocQ` (pp gpp :: MonoPP Priority)
      `adhocQ` (pp gpp :: MonoPP Priorities)
      `adhocQ` (pp gpp :: MonoPP IntCon)
      `adhocQ` (pp gpp :: MonoPP Renamings)
      `adhocQ` (pp gpp :: MonoPP Renaming)
      `adhocQ` (pp gpp :: MonoPP Definition)
      `adhocQ` (pp gpp :: MonoPP Module)
      `adhocQ` (pp gpp :: MonoPP Section)
      `adhocQ` (pp gpp :: MonoPP Sections)
      `adhocQ` (pp gpp :: MonoPP ModuleName)
      `adhocQ` (pp gpp :: MonoPP ImpSection)
      `adhocQ` (pp gpp :: MonoPP Imports)
      `adhocQ` (pp gpp :: MonoPP Import)
      `adhocQ` (pp gpp :: MonoPP Symbols)
      `adhocQ` (pp gpp :: MonoPP Attributes)
      `adhocQ` (pp gpp :: MonoPP Productions)
      `adhocQ` (pp gpp :: MonoPP SDF)
      `adhocQ` (pp gpp :: MonoPP UQLiteral)