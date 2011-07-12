:- ['../lost.pl'].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Simple length range model
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

simple_range_model_learn(DatabaseFile,OutFile) :-
	run_model(orf_length,simple_range_model_learn([DatabaseFile],[],OutFile)),
	write('params written to outfile: '), write(OutFile),nl.

simple_range_model_annotate(OrfsFile,ParametersFile) :-
	run_model(orf_length,simple_range_model_annotate([OrfsFile,ParametersFile],[],AnnotationFile)),
	write('Annotations written to file: '), write(AnnotationFile),nl.
	
simple_range_model_test :-
	lost_sequence_file('U00096_ptt',PTTFile),
    run_model(parser_ptt,annotate([PTTFile],[],DatabaseFile)),
	simple_range_model_learn(DatabaseFile,ParametersFile),
	lost_sequence_file('test_orfs',OrfsFile),
	simple_range_model_annotate(OrfsFile,ParametersFile).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ADPH length model
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

adph_model_learn(DatabaseFile,OutFile) :-
	run_model(orf_length,adph_model_learn([DatabaseFile],[],OutFile)),
	write('params written to outfile: '), write(OutFile),nl.

adph_model_annotate(OrfsFile,ParametersFile) :-
	run_model(orf_length,adph_model_annotate([OrfsFile,ParametersFile],[],AnnotationFile)),
	write('Annotations written to file: '), write(AnnotationFile),nl.

adph_model_test :-
	lost_sequence_file('U00096_ptt',PTTFile),
    run_model(parser_ptt,annotate([PTTFile],[],DatabaseFile)),
	adph_model_learn(DatabaseFile,ParametersFile),
	lost_sequence_file('test_orfs',OrfsFile),
	adph_model_annotate(OrfsFile,ParametersFile).

adph_distribution :-
	lost_sequence_file('U00096_ptt',PTTFile),
    run_model(parser_ptt,annotate([PTTFile],[],DatabaseFile)),
	adph_model_learn(DatabaseFile,ParametersFile),
	run_model(orf_length,adph_test_probs([ParametersFile],[],OutputFile)),
	write('length distribution written to file: '), write(OutputFile),nl.
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Old stuff
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%orfs(ecoli_all_orfs).
orfs(test_orfs).

test_learn(OutFile) :-
	%lost_sequence_file('U00096_ptt',PTTFile),n
	lost_sequence_file('salmonella_ptt',PTTFile),
        run_model(parser_ptt,annotate([PTTFile],[],DatabaseFile)),
	run_model(orf_length,learn([DatabaseFile],[],OutFile)),
	write('params written to outfile: '), write(OutFile),nl.

test_annotate :-
	test_learn(ParametersFile),
	orfs(Orfs),
	lost_sequence_file(Orfs,OrfsFile),
	run_model(orf_length,annotate([OrfsFile,ParametersFile],[],AnnotationFile)),
	write('Annotations written to file: '), write(AnnotationFile),nl.
	

test_learn_and_filter(OutFile) :-
	lost_sequence_file('U00096_ptt',PTTFile),
    run_model(parser_ptt,annotate([PTTFile],[],GoldenStandardFile)),
	lost_sequence_file(consorf_predictions_all,PredictionsFile),
	run_model(orf_length,learn([GoldenStandardFile,PredictionsFile],[],OutFile)),
	run_model(orf_length,filter_predictions([])).
	