:- ['../lost.pl'].

test :-
	lost_sequence_file('U00096_ptt',PTTFile),
    run_model(parser_ptt,annotate([PTTFile],[],DatabaseFile)),
	run_model(orf_length,learn([DatabaseFile],[],OutFile)),
	write('params written to outfile: '), write(OutFile),nl.