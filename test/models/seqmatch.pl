% Testing the seqmatch model

:- use(script).
:- use(prologdb).
:- use(fileformat).

create_fasta_file(F) :-
	lost_tmp_file('test-seqmatch',F),
	tell(F),
	writeln('> fasta blah blah\n'),
	writeln('AGCTACATGGT\n'),
	told.


% A rule to run the model
matchfile <- create_fasta_file(F) | seqmatch::match(file(F),[sequences([[t,a,c]])]).

testcase(seqmatch) :-
	run(matchfile),
	get_result_file(matchfile,File),
	file_exists(File),
	check_format(text(prolog(ranges(gene))),File).
	
	