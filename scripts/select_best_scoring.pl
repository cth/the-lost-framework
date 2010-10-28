:- ['../lost.pl'].


test :-
    lost_data_file('nc000913_2_all_urfs_for_real.pl',PredictionsFile),
    lost_data_file('nc000913_2_vecocyc_ptt',PttFile),
    run_model(parser_ptt,annotate([PttFile],[],GeneFileProlog)),
    terms_from_file(GeneFileProlog,CorrectGenes),
    length(CorrectGenes,NumberOfCorrectGenes),

%        run_model(select_best_scoring, annotate([PredictionsFile], [score_functor(score),number_of_predictions(2467)], SelectedPredictionsFile)),
    run_model(select_best_scoring, annotate([PredictionsFile], [score_functor(score),number_of_predictions(NumberOfCorrectGenes)], SelectedPredictionsFile)),
    run_model(accuracy_report, annotate([GeneFileProlog,SelectedPredictionsFile],[start(1),end(max)],AccuracyReport)),
    write('Accuracy report is : '), write(AccuracyReport), nl,
    run_model(accuracy_report, annotate([GeneFileProlog,SelectedPredictionsFile],[start(1),end(max)],AccuracyReport)),
    write('Accuracy report is : '), write(AccuracyReport), nl,
    readFile(AccuracyReport,Contents),
    atom_codes(Atom,Contents),
    write(Atom),
    nl.

data_points(5).
% The idea with this is to extract and experiment with all relevant 
% thresholds from the predictions in order to generate the data needed
% to produce a ROC curve.
% - Extract all the scores
%   - Iterate over all scores
%     - filter genes
%     - run accuracy
%     - consult report to extract TPR+FPR
%     - build up TPR+FPR in list
generate_roc_curve_data :-
	data_points(DataPoints),
    lost_data_file('nc000913_2_all_urfs_for_real.pl',PredictionsFile),
%    lost_data_file('nc000913_2_1000_orfs.pl',PredictionsFile),
    lost_data_file('nc000913_2_vecocyc_ptt',PttFile),
    run_model(parser_ptt,annotate([PttFile],[],GeneFileProlog)),
    terms_from_file(PredictionsFile,Predictions),
    % Extract all unique scores:
    findall(Score,(member(P,Predictions),score_wrap(score,P,[Score,P])),Scores),
	list_max(Scores,MaxScore),
	list_min(Scores,MinScore),
	write(create_data_points(MinScore,MaxScore,DataPoints,DataPointsList)),nl,
	create_data_points(MinScore,MaxScore,DataPoints,DataPointsList),
	write(DataPointsList),nl,
	% Do cut-offs at each reported score and report accuracy for that cutoff
	select_and_report(GeneFileProlog,PredictionsFile,DataPointsList,Results),
	write('Writing results file'),nl,
	open('results.txt',write,OStream),
	report_results_as_tabsep(OStream,Results),
	close(OStream).
	
report_results_as_tabsep(_,[]).
report_results_as_tabsep(Stream,[[Score,AccReport]|Rest]) :-
	write(Stream,Score),
	write(Stream,'\t'),
	member(accuracy_report(gene_stops_correct,GeneStopsCorrect),AccReport),
	write(Stream,GeneStopsCorrect),
	write(Stream,'\t'),
	member(accuracy_report(gene_stops_wrong,GeneStopsWrong),AccReport),
	write(Stream,GeneStopsWrong),
	write(Stream,'\n'),
	report_results_as_tabsep(Stream,Rest).

	
create_data_points(_,_,0,[]).
create_data_points(Min,Max,DataPoints,[Min|DPs]) :-
	Range is Max - Min,
	Interval is Range / DataPoints,
	NextMin is Min + Interval,
	NextDataPoints  is DataPoints - 1,
	create_data_points(NextMin,Max,NextDataPoints,DPs).
	
	
select_and_report(_,_,[],[]).
select_and_report(GeneFileProlog,PredictionsFile,[Score|ScoresRest],[[Score,AccuracyReport]|AccRest]) :-
	write('Score cutoff: '), write(Score), nl,
    run_model(select_best_scoring, annotate([PredictionsFile], [score_threshold(Score),score_functor(score)], SelectedPredictionsFile)),
    run_model(accuracy_report, annotate([GeneFileProlog,SelectedPredictionsFile],[start(1),end(max),reports([gene_stops_correct,gene_stops_wrong])],AccuracyReportFile)),
	terms_from_file(AccuracyReportFile,AccuracyReport),
	select_and_report(GeneFileProlog,PredictionsFile,ScoresRest,AccRest).

score_wrap(ScoreFunctor,Prediction,[Score,Prediction]) :-
    Prediction =.. [ _Functor, _Id, _Left, _Right, _Strand, _Frame, Extra ], 
    ScoreMatcher =.. [ ScoreFunctor, Score ],
    member(ScoreMatcher,Extra).



% dra