#################

EXTRA_DIST = \
	CobolSyntax.def CobolSyntax.hs \
	CobolSyntaxTermInstances.hs CobolSyntaxATermConvertibleInstances.hs \
	CobolLib.hs TestParser.hs PerformGraph.hs

SUBDIRS	= testdata	

Sdf2Haskell=${top_srcdir}

GHCOPTS = 
include ${top_srcdir}/scripts/makerules.misc
GHCOPTS += -package HaXml -i:${top_srcdir}/generator

BINS	= TestParser PerformGraph

Generated = \
	CobolSyntax.tbl CobolSyntax.hs CobolSyntaxTermInstances.hs \
	CobolSyntaxATermConvertibleInstances.hs

ParseTests = \
	${patsubst %.cob, %.parse, ${wildcard testdata/*.cob}}
	
PerformGraphTests = \
	${patsubst %.cob, %.pg.test, ${wildcard testdata/*.cob}}
	
Tests	= ${ParseTests} ${PerformGraphTests}

##################

check-local: generate compile-all ${Tests}

generate: ${Generated}
	
compile-all: configuration generate ${BINS}

################

## Rules for testing

%.parse: %.cob
	./TestParser $< > /dev/null
	
%.pg.test: 
	@${RM} $*.pg.dot
	@${MAKE} $*.pg.dot
	diff $*.pg.dot $*.pg.dot.correct

%.pg.dot: %.cob
	./PerformGraph $< > $@

%.asfix: %.cob.trm CobolSyntax.tbl
	sglr -2 -v -p CobolSyntax.tbl -i $< -o $@

%.asfix: %.cob CobolSyntax.tbl
	sglr -2 -v -p CobolSyntax.tbl -i $< -o $@

%.af: %.asfix
	implode-asfix --lex --layout --alt --appl --inj --list  --seq --lit --pt -i $< -o $@

%.eps: %.dot
	dot $< -Tps -o $@

%.pdf: %.eps
	ps2pdf $< $@


##################

CLEANFILES = \
	*.hi *.o *~ *.asfix *.af *.tbl *.dot *.ps \
	${BINS}
	
very-clean: clean
	${RM} CobolSyntax.hs Cobol*Instances.*

###################
