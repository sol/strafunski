ATermLib = ..

include ${ATermLib}/scripts/makerules.clean
include ${ATermLib}/scripts/makerules.recurse
include ${ATermLib}/scripts/makerules.opts

check-ghci:
	echo "" | ghci ${ghc-opts} ATermLib

check-ghc:
	ghc ${ghc-opts} --make ATermLib

check-hugs:
	echo "" | hugs ${hugs-opts} ATermLib

