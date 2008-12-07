module Sdf where

import ATermLib
import TermRep
 
data Grammar = Sdf_aliases Aliases
	     | Sdf_restrictions Restrictions
	     | Sdf_sorts_ Symbols
	     | Sdf_priorities Priorities
	     | Sdf_imp_section ImpSection
	     | Sdf_lexical_syntax Productions
	     | Sdf_context_free_syntax Productions
	     | Sdf_variables Productions
	     | Sdf_lexical_variables Productions
	     | Sdf_empty_grammar
	     | Sdf_conc_grammars Grammar Grammar
	     | Sdf_syntax Productions
	     | Sdf_lexical_priorities Priorities
	     | Sdf_context_free_priorities Priorities
	     | Sdf_lexical_restrictions Restrictions
	     | Sdf_context_free_restrictions Restrictions
 
data Alias = Sdf_alias Symbol Symbol
 
data Aliases = Sdf_list [Alias]
 
data Lookahead = Sdf_char_class CharClass
	       | Sdf_seq CharClass Lookaheads
 
data Lookaheads = Sdf_single Lookahead
		| Sdf_alt Lookaheads Lookaheads
		| Sdf_list1 [Lookahead]
 
data Restriction = Sdf_follow Symbols Lookaheads
 
data Restrictions = Sdf_list2 [Restriction]
 
data Attribute = Sdf_reject
	       | Sdf_prefer
	       | Sdf_avoid
	       | Sdf_cons1 ATerm'
	       | Sdf_constructor
	       | Sdf_memo
	       | Sdf_traverse
	       | Sdf_bracket
	       | Sdf_atr Associativity
	       | Sdf_id ModuleName
 
data OptExp = Sdf_present IntCon
	    | Sdf_absent
 
data RealCon = Sdf_real_con IntCon NatCon OptExp
 
data AFun = Sdf_Literal Literal
 
data ATerm' = Sdf_fun AFun
 
data Symbol = Sdf_label Literal Symbol
	    | Sdf_lit Literal
	    | Sdf_sort Sort
	    | Sdf_char_class1 CharClass
	    | Sdf_empty1
	    | Sdf_seq1 Symbol [Symbol]
	    | Sdf_opt Symbol
	    | Sdf_iter Symbol
	    | Sdf_iter_star Symbol
	    | Sdf_iter_sep Symbol Symbol
	    | Sdf_iter_star_sep Symbol Symbol
	    | Sdf_iter_n Symbol NatCon
	    | Sdf_iter_sep_n Symbol Symbol NatCon
	    | Sdf_set Symbol
	    | Sdf_pair Symbol Symbol
	    | Sdf_func Symbols Symbol
	    | Sdf_alt1 Symbol Symbol
	    | Sdf_perm Symbols
	    | Sdf_cf Symbol
	    | Sdf_lex Symbol
	    | Sdf_varsym Symbol
	    | Sdf_layout
	    | Sdf_start
	    | Sdf_file_start
 
data Literal = Sdf_quoted QLiteral
	     | Sdf_uqlit UQLiteral
 
data Production = Sdf_prod_fun Literal [Symbol] Symbol Attributes
		| Sdf_prod Symbols Symbol Attributes
 
data Character = Sdf_numeric NumChar
	       | Sdf_short ShortChar
	       | Sdf_top
	       | Sdf_eof
	       | Sdf_bot
	       | Sdf_label_start
 
data CharRange = Sdf_Character Character
	       | Sdf_range Character Character
 
data CharRanges = Sdf_CharRange CharRange
		| Sdf_conc CharRanges CharRanges
 
data OptCharRanges = Sdf_absent1
		   | Sdf_present1 CharRanges
 
data CharClass = Sdf_simple_charclass OptCharRanges
	       | Sdf_comp CharClass
	       | Sdf_diff CharClass CharClass
	       | Sdf_isect CharClass CharClass
	       | Sdf_union CharClass CharClass
 
data Associativity = Sdf_left
		   | Sdf_right
		   | Sdf_non_assoc
		   | Sdf_assoc
 
data Group = Sdf_simple_group Production
	   | Sdf_prods_group Productions
	   | Sdf_assoc_group Associativity Productions
 
data Priority = Sdf_chain [Group]
	      | Sdf_assoc1 Group Associativity Group
 
data Priorities = Sdf_comma [Priority]
 
data IntCon = Sdf_natural NatCon
	    | Sdf_positive NatCon
	    | Sdf_negative NatCon
 
data Renamings = Sdf_renamings [Renaming]
 
data Renaming = Sdf_symbol Symbol Symbol
	      | Sdf_production Production Production
 
data Definition = Sdf_list4 [Module]
 
data Module = Sdf_module_ ModuleName [ImpSection] Sections
 
data Section = Sdf_exports_ Grammar
	     | Sdf_hiddens Grammar
 
data Sections = Sdf_list5 [Section]
 
data ModuleName = Sdf_unparameterized ModuleId
		| Sdf_parameterized ModuleId Symbols
 
data ImpSection = Sdf_imports_ Imports
 
data Imports = Sdf_list6 [Import]
 
data Import = Sdf_module1 ModuleName
	    | Sdf_renamed_module ModuleName Renamings
 
data Symbols = Sdf_list7 [Symbol]
 
data Attributes = Sdf_attrs [Attribute]
		| Sdf_no_attrs
 
data Productions = Sdf_list8 [Production]
 
data SDF = Sdf_definition Definition
 
type AlphaNumericalEscChar = String
 
type DecimalEscChar = String
 
type EscChar = String
 
type L_Char = String
 
type QLiteral = String
 
type UQLiteral = String
 
type Sort = String
 
type NumChar = String
 
type ShortChar = String
 
type NatCon = String
 
type ModuleWord = String
 
type ModuleId = String