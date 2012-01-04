:- ['../../lost.pl'].
:- lost_include_api(interface).
:- lost_include_api(misc_utils).
:- lost_include_api(genedb).
:- lost_include_api(io).

:- task(filter([text(prolog(ranges(gene)))],[prediction_functor(auto),score_functor(not_set)],text(prolog(ranges(gene))))).

lost_option(annotate,prediction_functor,auto,'The functor of the facts used in the prediction file. The default value \"auto\" means that the model will try to figure it out automatically').
lost_option(annotate,score_functor,not_set,'This model expects a fact in the extra list argument, which contains a score for the each gene prediction. score_functor determines the functor of this fact.').

lost_input_formats(annotate, [text(prolog(ranges(gene)))]).
lost_output_format(annotate, _, text(prolog(ranges(gene)))).


%% filter(+InputFiles,+Options,+OutputFile)
% For each distinct stop codon in the predictions in the the input file, write prediction which has has the highest
% score to the output file. 
% The functor of predictions in the input file will be automatically inferred if option =|prediction_functor=auto|= and the file contains only the same type of functor. Otherwise the value of =|prediction_functor|= should be set.
% The =|score_functor|= option _must_ be set. The predictions are expected to have an extra field indicating the score of a prediction as numeric value, .e.g. in the example below, the you would use =|score_functor(score)|=.
% ==
% prediction(org,123,456,+,1,[score(0.99)])
% ==
filter([InputFile],Options,OutputFile) :-
        write('--------------------  start'),nl,
	consult(InputFile),
	get_option(Options,prediction_functor,PredFunctorOpt),
	((PredFunctorOpt == auto) -> file_functor(InputFile,PredFunctor) ;  PredFunctor = PredFunctorOpt),
	get_option(Options,score_functor,ScoreFunctor),
	((ScoreFunctor == not_set) -> throw(error(score_functor_must_be_set)) ; true),
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
        gene_extra_field(Prediction1,ScoreFunctor,P1),
        gene_extra_field(Prediction2,ScoreFunctor,P2),
	((P1 > P2) ->
	 select_prediction(ScoreFunctor,[Prediction1|Rest],BestPrediction)
	;
	 select_prediction(ScoreFunctor,[Prediction2|Rest],BestPrediction)
	).
