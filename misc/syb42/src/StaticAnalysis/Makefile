ghci = ghci -v0
all:
	echo main | ${ghci} Examples/Company/Interpreter.hs
	echo main | ${ghci} AnalysisLib/SuccessFailure/AbstractInterpretation.hs
	echo main | ${ghci} AnalysisLib/SuccessFailure/TypeSystem.hs
	echo main | ${ghci} Examples/Company/Reachability.hs
	echo main | ${ghci} AnalysisLib/Termination/DepthMeasure.hs
	echo main | ${ghci} AnalysisLib/Termination/CompoundMeasure.hs
