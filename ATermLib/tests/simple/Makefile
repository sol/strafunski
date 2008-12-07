ATermLib = ../..

include ${ATermLib}/scripts/makerules.clean
include ${ATermLib}/scripts/makerules.recurse
include ${ATermLib}/scripts/makerules.opts

check-hugs:
	hugs ${hugs-opts} -P${ATermLib}/library: Test.hs < Test.script

check-ghci:
	ghci ${ghc-opts} -i${ATermLib}/library: Test.hs < Test.script

check-ghc:

#######################################################
#
# For developers only:

regenerate-tests: Test.hs

%.hs: %.ds
	DrIFT $< > $@ 
	
#######################################################
