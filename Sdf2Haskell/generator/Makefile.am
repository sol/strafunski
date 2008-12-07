#############

# Generated files are included in the distribution, when possible.
EXTRA_DIST = \
	Sdf.def \
	Sdf.ds Sdf.hs SyntaxATermConvertibleInstances.hs \
	SdfMetrics.hs Sdf2Haskell.hs HaskellLib.hs SdfLib.hs \
	Sdf2HaskellUtils.hs Sdf2Pretty.hs Sdf2Syntax.hs \
	SGLR.hs GPP.hs SdfPP.hs \
	Sdf2Cfg.hs Cfg.hs ImplodePT.hs

Sdf2Haskell=${top_srcdir}
include ${top_srcdir}/scripts/makerules.misc

##############

all: compile-all

compile-all: generate Configuration.hs Sdf2Haskell #sdf2haskell

sdf2haskell: sdf2haskell.src
	sed s@__Sdf2Haskell__@$(PWD)@g $< > $@
	chmod ug+x $@

CLEANFILES = \
	*.hi *.o *~ *.af *.out *.tbl *.dot *.ps \
	Sdf2Haskell \
	sdf2haskell
	
## Only needed by developers:	

generate:
	$(MAKE) Sdf.tbl
	$(MAKE) Sdf.hs
	$(MAKE) SyntaxATermConvertibleInstances.hs

Sdf.hs: Sdf.ds
	${DRIFT} -g ATermConvertible -g Term -g Typeable -g Show Sdf.ds | \
   sed s/mkAppTy/mkTyConApp/g >> Sdf.hs
	
# find source of Language.Haskell.Syntax
Syntax      = ${shell locate -n1 libraries/Language/Haskell/Syntax.hs}

SyntaxATermConvertibleInstances.hs: ${Syntax}
	echo "module SyntaxATermConvertibleInstances where" > $@
	echo "import Language.Haskell.Syntax" >> $@
	echo "import ATermLib" >> $@
	${DRIFT} -g ATermConvertible -r $< >> $@

very-clean: clean
	${RM} Sdf.hs SyntaxATermConvertibleInstances.hs
	
## For debugging purposes:	

echo-%:
	@echo $($*)

##########################
