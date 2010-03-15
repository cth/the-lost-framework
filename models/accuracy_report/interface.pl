:- ['../../lost.pl'].
:- lost_include_api(interface).
:- lost_include_api(io).
:- lost_include_api(accuracy).

lost_best_annotation([ReferenceFile,PredictionFile],Options,OutputFile) :-
	lost_required_option(Options,reference_functor,ReferenceFunctor),
	lost_required_option(Options,prediction_functor,PredictionFunctor),
	lost_required_option(Options,start,Start),
	lost_required_option(Options,end,End),
	check_or_fail(consult(ReferenceFile),error(cannot_consult(ReferenceFile))),
	check_or_fail(consult(PredictionFile),error(cannot_consult(PredictionFile))),
%	check_or_fail(
%		      accuracy_stats(ReferenceFunctor,PredictionFunctor,Start,End,OutputFile),
%		      error(problem_generating_accuracy_stats)).
        write(accuracy_stats(ReferenceFunctor,PredictionFunctor,Start,End,OutputFile)),
        accuracy_stats(ReferenceFunctor,PredictionFunctor,Start,End,OutputFile).
		 
