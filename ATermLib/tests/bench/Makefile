ATermLib = ../..

include ${ATermLib}/scripts/makerules.clean
include ${ATermLib}/scripts/makerules.recurse
include ${ATermLib}/scripts/makerules.opts
include ${ATermLib}/scripts/makerules.compile

# We need some additional options
ghc-opts += -i${ATermLib}/library:

benchfiles = TwoSorts.def.af yacc.def.af sdf.2.3-I.def.af

check-hugs:
	@echo "Benchmarking for Hugs ..."
	@echo "" > bench-hugs.log
	@$(MAKE) check-hugs-TwoSorts check-hugs-yacc check-hugs-sdf.2.3-I
	@echo "Benchmark results for Hugs in bench-hugs.log"

check-hugs-%:
	@$(MAKE) \
	  benchlog=bench-hugs.log \
	  benchfile=$*.def.af \
	  command="runhugs $(hugs-opts) -h2500000 -P${ATermLib}/library: ATermBench.hs" \
	  bench

check-ghci:

check-ghc: ATermBench
	@echo "Benchmarking for GHC ..."
	@echo "" > bench-ghc.log
	@$(MAKE) check-ghc-TwoSorts check-ghc-yacc check-ghc-sdf.2.3-I
	@echo "Benchmark results for GHC in bench-ghc.log"

check-ghc-%:
	@$(MAKE) \
	  benchlog=bench-ghc.log \
	  benchfile=$*.def.af \
	  command="./ATermBench" \
	  bench

#######################################################
#
# Benchmark rules
#	parameters:
#		benchlog
#		benchfile
#		command

bench: bench-trm2taf bench-taf2trm

bench-trm2taf:
	@echo "Bench trm2taf for $(benchfile)" >> $(benchlog)
	@(time -p $(command) -s -i $(benchfile).trm -o /dev/null) 2>> $(benchlog)

bench-taf2trm:
	@echo "Bench taf2trm for $(benchfile)" >> $(benchlog)
	@(time -p $(command) -t -i $(benchfile).taf -o /dev/null) 2>> $(benchlog)


#######################################################
#
# For developers only:

regenerate-tests: $(benchfile).taf $(benchfile).trm
	
%.taf: %
	trm2taf -i $< -o $@

%.trm: %
	trmcat -i $< -o $@

#######################################################