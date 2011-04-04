:- ['../lost.pl'].

%orfs(ecoli_all_orfs).
orfs(test_orfs).

test_learn(OutFile) :-
	%lost_sequence_file('U00096_ptt',PTTFile),
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
	
