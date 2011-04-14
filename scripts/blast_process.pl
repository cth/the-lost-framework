:- ['../lost.pl'].

test :-
	lost_data_directory(DD),
	atom_concat(DD,'test_blast_trim.pl', BlastFile),
	run_model(stop_trimmer,trim_stops([BlastFile],[],OutFile)),
	write('outfile is :'), write(OutFile), nl.

idseq :-
	lost_data_directory(DD),
	atom_concat(DD,'test_blast_trim.pl', BlastFile),
	run_model(process_blast_matches,identity([BlastFile],[],OutFile)),
	write('outfile is :'), write(OutFile), nl.
