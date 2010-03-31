% Utilities for working with the genedb format.

genedb_distinct_predictions(_,[],[]).

genedb_distinct_predictions(PredFunctor,[DistinctEnd|RestEnds],[PredictionsForEnd|RestPred]) :-
	genedb_predictions_for_stop_codon(PredFunctor,DistinctEnd,PredictionsForEnd),
	genedb_distinct_predictions(PredFunctor,RestEnds,RestPred).

genedb_distinct_stop_codons(PredFunctor,DistinctStops) :-
	ForwardStrand =.. [ PredFunctor, _,StopCodonEnd,'+',Frame,_],
	ReverseStrand =.. [ PredFunctor, StopCodonEnd,_,'-',Frame,_],
	findall([StopCodonEnd,'+',Frame], ForwardStrand, ForwardStops),
	findall([StopCodonEnd,'-',Frame], ReverseStrand, ReverseStops),
	append(ForwardStops,ReverseStops,AllStops),
	eliminate_duplicate(AllStops,DistinctStops).

genedb_predictions_for_stop_codon(PredFunctor,[StopCodonEnd,'+',Frame],Predictions) :-
	FindGoal =.. [ PredFunctor, Start,StopCodonEnd,'+',Frame,Extra],
	BuildGoal =.. [ PredFunctor, Start,StopCodonEnd,'+',Frame,Extra],
	findall(BuildGoal,FindGoal,Predictions).

genedb_predictions_for_stop_codon(PredFunctor,[StopCodonEnd,'-',Frame],Predictions) :-
	FindGoal =.. [ PredFunctor, StopCodonEnd,End,'-',Frame,Extra],
	BuildGoal =.. [ PredFunctor, StopCodonEnd,End,'-',Frame,Extra],
	findall(BuildGoal,FindGoal,Predictions).

