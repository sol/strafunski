###############################################################################
#
# Environment for makerules
#

ATermLib    = .
name	    = haterm
subdirs     = library scripts tests
version     = ${shell cat VERSION}

###############################################################################
#
# Create awareness of different checking options
#

check:
	@echo
	@echo " Options:"
	@echo "  Enter \"${MAKE} check-ghci\" to check with ghci."
	@echo "  Enter \"${MAKE} check-ghc\"  to check with ghc."
	@echo "  Enter \"${MAKE} check-all\"  to check everything, i.e.:"
	@echo "         - check with ghci, and ghc"
	@echo "         - regenerate all derived instances with DrIFT 2.0.1"
	@echo



###############################################################################
#
# Check everything
#

check-all:
	${MAKE} very-clean
	${MAKE} check-ghci
	${MAKE} check-ghc


###############################################################################
#
# Build a distribution
#

dist:
	@${RM} -r /tmp/$(name)-${version}
	@ln -s ${PWD} /tmp/$(name)-${version}
	@(cd /tmp;\
	  zip -q -r $(name)-${version}.zip \
	   `find ${name}-${version} -follow \
             -name Makefile -or -name "makerules.*" -or \
	     -name "*.hs" -or -name "*.lhs" -or\
	     -name "*.html" -or -name "*.css" -or -name "haskell_icon.gif" -or \
	     -name README -or -name AUTHORS -or -name Prologue -or \
	     -name ChangeLog -or -name INSTALL -or -name VERSION -or \
	     -name TODO -or -name Test.hs -or -name "Test.script" -or \
	     -name "*.def.*" | grep -v "/out/" | grep -v "out.hs"` \
	 )
	@${RM} -r /tmp/${name}-${version}
	@echo "Created distribution in: /tmp/${name}-${version}.zip"

distcheck:
	$(MAKE) dist
	@(cd /tmp; \
	  unzip -q /tmp/${name}-${version}.zip \
	 )
	@(cd /tmp/${name}-${version}; \
	  ${MAKE} check-all \
	 )
	@${RM} -rf /tmp/${name}-${version}
	@echo "****************"
	@echo "/tmp/${name}-${version}.zip is ready for distribution"
	@echo "****************"


###############################################################################
#
# Documentation
#

docDir		= ./documentation

haddock:
	@$(RM) -r ${docDir}
	@mkdir -p ${docDir}
	@echo "Running Haddock, redirecting warnings to haddock.log ..."
	@haddock -o ${docDir} -h \
	  --title="Haskell ATerm Library ($(version))" \
	  -p Prologue \
	  `find ./library -name "*.hs" -or -name "*.lhs"` 2> haddock.log
	@echo "Generated documentation in $(docDir)"


###############################################################################
#
# Reusable includes
#

include ${ATermLib}/scripts/makerules.clean
include ${ATermLib}/scripts/makerules.check
include ${ATermLib}/scripts/makerules.recurse

