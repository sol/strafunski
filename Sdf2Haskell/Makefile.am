###############################################################################

SUBDIRS	= generator scripts examples

EXTRA_DIST = VERSION configure.in \
             reconf acincludepackage.m4 \
	     ${wildcard ./documentation/*}

RTS = generator/GPP.hs generator/SGLR.hs generator/ImplodePT.hs

###############################################################################

all: compile-all

compile-all:
	cd ./generator;$(MAKE) compile-all
	cd ./examples/java;$(MAKE) compile-all
	cd ./examples/vs-cobol-ii;$(MAKE) compile-all

simple-distcheck:
	$(MAKE) dist
	gunzip -c $(PACKAGE)-$(VERSION).tar.gz | tar xf -
	( cd $(PACKAGE)-$(VERSION);\
	  ./configure --prefix=$(PWD)/$(PACKAGE)-$(VERSION)/install\
	              --with-haterm=$(haterm)\
		      --with-strategylib=$(strategylib)\
		      --with-drift=$(DRIFT);\
	  $(MAKE) all check;\
	)
	zip $(PACKAGE)-rts-$(VERSION).zip $(patsubst %, $(PACKAGE)-$(VERSION)/%, $(RTS))
	rm -rf $(PACKAGE)-$(VERSION)
	@echo Successfull: simple-distcheck
	@echo ========================================================
	@echo == $(PACKAGE)-$(VERSION).tar.gz ready for distribution
	@echo ========================================================
	@echo == Runtime support in: $(PACKAGE)-rts-$(VERSION).zip
	@echo ========================================================

CLEANFILES = *.tar.gz *.pkg *.zip


###############################################################################
#
# Bundle runtime support

rts:
	$(MAKE) dist
	gunzip -c $(PACKAGE)-$(VERSION).tar.gz | tar xf -
	zip $(PACKAGE)-rts-$(VERSION).zip $(patsubst %, $(PACKAGE)-$(VERSION)/%, $(RTS))
	rm -rf $(PACKAGE)-$(VERSION)
	@echo ========================================================
	@echo == Runtime support in: $(PACKAGE)-rts-$(VERSION).zip
	@echo ========================================================


###############################################################################
#
# Documentation
#

docDir		= ./documentation

haddock:
	mkdir -p ${docDir}
	haddock -o ${docDir} -h --title=Sdf2Haskell \
	  `find ./generator -name "*.hs" -or -name "*.lhs"`


###############################################################################
#
# For debugging purposes

echo-%:
	@echo $($*)

###############################################################################



