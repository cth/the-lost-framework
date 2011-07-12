:- ['../lost.pl'].

:- lost_include_api(io).

ref_database_file(PTTName,DatabaseFile) :-
	lost_sequence_file(PTTName,PTTFile),
	run_model(parser_ptt,annotate([PTTFile],[],DatabaseFile)).


test :-
	lost_sequence_file('mini_test',ChunkFile),
	ref_database_file('NC_000913_ptt',RefFile),
	run_model(chunk_ref_annot,add_reference_track([ChunkFile,RefFile],[],OutFile)),
	write('results to : '), write(OutFile), nl.


