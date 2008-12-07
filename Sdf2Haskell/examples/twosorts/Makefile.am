#############

EXTRA_DIST = \
	TwoSorts.def TwoSorts.hs.correct
	
Sdf2Haskell=${top_srcdir}
include ${top_srcdir}/scripts/makerules.misc

##############

check-local: compile-all
	diff TwoSorts.hs TwoSorts.hs.correct

generate:
	$(MAKE) TwoSorts.hs

compile-all: generate

CLEANFILES = \
	*~ TwoSorts.hs *.o *.hi

###############
