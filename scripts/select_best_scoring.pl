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
