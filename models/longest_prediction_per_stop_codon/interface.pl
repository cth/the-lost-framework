:- ['../../lost.pl'].
:- lost_include_api(interface).
:- lost_include_api(misc_utils).
:- lost_include_api(genedb).
:- lost_include_api(io).

lost_option(annotate,prediction_functor,auto,'The functor of the facts used in the prediction file. The default value \"auto\" means that the model will try to figure it out automatically').

lost_input_formats(annotate, [text(prolog(ranges(gene)))]).
lost_output_format(annotate, _, text(prolog(ranges(gene)))).


annotate([InputFile],Options,OutputFile) :-
	consult(InputFile),
	get_option(Options,prediction_functor,PredFunctorOpt),
	((PredFunctorOpt == auto) -> file_functor(InputFile,PredFunctor) ;  PredFunctor = PredFunctorOpt),
	genedb_distinct_stop_codons(PredFunctor,DistinctStops),
	length(DistinctStops,DSL),
	write('distinct stops: '), write(DSL),nl,
	genedb_distinct_predictions(PredFunctor,DistinctStops,DistinctPredictions),
	length(DistinctPredictions,DSP),
	write('distinct predictions: '), write(DSP),nl,	
	map(select_prediction,DistinctPredictions,BestPredictions),
	write('Writing '),
	length(BestPredictions,NumberOfPredictions),
	write(NumberOfPredictions),
	write(' predictions to file '),
	write(OutputFile),
	nl,
	terms_to_file(OutputFile,BestPredictions).

select_prediction([BestPrediction],BestPrediction).

select_prediction([Prediction1,Prediction2|Rest],BestPrediction) :-
	Prediction1 =.. [_,Start1,End1,_,_,_],
	Prediction2 =.. [_,Start2,End2,_,_,_],
	Diff1 is End1 - Start1,
	Diff2 is End1 - Start2,
        abs(Diff1,L1),
	abs(Diff2,L2),
	((L1 > L2) ->
	 select_prediction([Prediction1|Rest],BestPrediction)
	;
	 select_prediction([Prediction2|Rest],BestPrediction)
	).

