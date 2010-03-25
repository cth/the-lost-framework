
:- ['../../lost.pl'].
:- lost_include_api(interface).
:- lost_include_api(misc_utils).
:- lost_include_api(genedb).

lost_best_annotation([InputFile],Options,OutputFile) :-
	consult(InputFile),
	lost_required_option(Options,prediction_functor,PredFunctor),
	lost_required_option(Options,score_functor,ScoreFunctor),
	genedb_distinct_stop_codons(PredFunctor,DistinctStops),
	length(DistinctStops,DSL),
	write('distinct stops: '), write(DSL),nl,
	genedb_distinct_predictions(PredFunctor,DistinctStops,DistinctPredictions),
	length(DistinctPredictions,DSP),
	write('distinct predictions: '), write(DSP),nl,	
	map(select_prediction(ScoreFunctor,input,output),DistinctPredictions,BestPredictions),
	write('Writing '),
	length(BestPredictions,NumberOfPredictions),
	write(NumberOfPredictions),
	write(' predictions to file '),
	write(OutputFile),
	nl,
	terms_to_file(OutputFile,BestPredictions).

select_prediction(_,[BestPrediction],BestPrediction).

select_prediction(ScoreFunctor,[Prediction1,Prediction2|Rest],BestPrediction) :-
	Prediction1 =.. [_,_,_,_,_,Extra1],
	Prediction2 =.. [_,_,_,_,_,Extra2],
	G1 =.. [ ScoreFunctor, P1 ],
	G2 =.. [ ScoreFunctor, P2 ],
	member(G1, Extra1),
	member(G2, Extra2),
	((P1 > P2) ->
	 select_prediction(ScoreFunctor,[Prediction1|Rest],BestPrediction)
	;
	 select_prediction(ScoreFunctor,[Prediction2|Rest],BestPrediction)
	).
