#############

EXTRA_DIST = \
	DTDJavaMetrics.hs \
	JavaIO.hs JavaUtil.hs JavaChaseImports.hs \
	JavaMetrics.hs Java2PCG.hs Java2CCG.hs JavaPPTest.hs \
	Java.def Hoi.java Hello.java ${wildcard *.correct} Verbatim.java \
	javametrics.dtd
	
Sdf2Haskell=${top_srcdir}

GHCOPTS =
include ${top_srcdir}/scripts/makerules.misc
GHCOPTS += -package HaXml -i${top_srcdir}/generator

BINS	= Java2PCG Java2CCG JavaMetrics JavaPPTest
TestPre = Hoi.java.af Hello.java.af 
Tests	= Hoi.metrics.test Hoi.pcg.test Hoi.ccg.test Verbatim.pp.test
Generated = DTDJavaMetricsTermInstances.hs JavaATermConvertibleInstances.hs \
            JavaTermInstances.hs Java.tbl Java.hs JavaPP.hs 
	     

##############

check-local: compile-all ${TestPre} ${Tests}

generate: ${Generated}

compile-all: configuration generate ${BINS}

##################

## Rules for testing

%.java.af: %.java 
	sglr -p Java.tbl -s CompilationUnit -2 -i $< | implode-asfix > $@

%.metrics.test: %.java.af
	./JavaMetrics $* > $*.metrics.xml
	diff $*.metrics.xml $*.metrics.xml.correct

%.ccg.test: %.java.af
	./Java2CCG $< > $*.ccg.dot
	dot  -Granksep=.25 -Nheight=.25 -Gnodesep=.1 -Tps $*.ccg.dot > $*.ccg.ps 
	@echo "Created Conditional Call Graph: $*.ccg.ps"
	diff $*.ccg.dot $*.ccg.dot.correct

%.pcg.test: %.java.af
	./Java2PCG $< > $*.pcg.dot
	dot  -Granksep=.25 -Nheight=.25 -Gnodesep=.1 -Tps $*.pcg.dot > $*.pcg.ps
	@echo "Created Package/Class Graph: $*.pcg.ps"
	diff $*.pcg.dot $*.pcg.dot.correct

%.pp.test: %.java
	./JavaPPTest $* > $<.pp
	diff $< $<.pp

##################

CLEANFILES = \
	*.hi *.o *~ *.asfix *.af *.tbl *.dot *.ps \
	${BINS}
	
very-clean: clean
	${RM} *Java*Instances.* Java.hs

##################
