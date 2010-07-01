% Test integration of various genefinders.

:- ['../lost.pl'].
:- lost_include_api(interface).
genbank_database_file(PTTName,DatabaseFile) :-
	lost_sequence_file(PTTName,PTTFile),
	run_model(parser_ptt,annotate([PTTFile],[],DatabaseFile)).

test :-
	lost_sequence_file('U00096_fna',SeqFile),
	run_model(genemark,
                  annotate([SeqFile],[parameters('Escherichia_coli_K12'),threshold(0.0)],PredictionsFile)),
        run_model(best_prediction_per_stop_codon,
                  annotate([PredictionsFile],
                                [prediction_functor(genemark_gene_prediction),
                                score_functor(start_codon_probability)],
                           TrimmedPredictions)),
   	write('Results are stored in: '),
	write(TrimmedPredictions),nl,

	genbank_database_file('U00096_ptt',ReferenceFile),
        run_model(accuracy_report,
                  annotate([ReferenceFile,TrimmedPredictions],[start(1)],AccuracyReportFile)),

   	write('Accuracy report in: '),
	write(AccuracyReportFile),nl,
	nl.

genemark_best_predictions(TrimmedPredictions) :-
	lost_sequence_file('U00096_fna',SeqFile),
	run_model(genemark,
                  annotate([SeqFile],[parameters('Escherichia_coli_K12'),threshold(0.0)],PredictionsFile)),
        run_model(best_prediction_per_stop_codon,
                  annotate([PredictionsFile],
                                [prediction_functor(genemark_gene_prediction),
                                score_functor(start_codon_probability)],
                           TrimmedPredictions)),
   	write('Results are stored in: '),
	write(TrimmedPredictions),nl.


test2 :-
        lost_data_directory(DataDir),
        write(DataDir),nl,
        atom_concat(DataDir, 'k12_verified_mRNAs.ptt', PTTFile), 
        write(PTTFile),nl,
        genemark_best_predictions(TrimmedPredictions),
	run_model(parser_ptt,annotate([PTTFile],[],RefsFile)),
        run_model(accuracy_report,
                  annotate([RefsFile,TrimmedPredictions],[start(1)],AccuracyReportFile)),
        atom_concat('less ', AccuracyReportFile, Cmd),
        system(Cmd).

