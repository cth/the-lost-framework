% Testing the seqmatch model

:- use(script).
:- use(prologdb).
:- use(fileformat).

create_fasta_file(F) :-
	lost_tmp_file('test-seqmatch',F),
	tell(F),
	writeln('> fasta blah blah\n'),
	writeln('AGCTACATGG\n'),
	told.


% A rule to run the model
matchfile <- create_fasta_file(F) | sequence::match(file(F),[sequences([[t,a,c]])]).

% Simple extract - forward strand
extract1 <- create_fasta_file(F) | sequence::extract(file(F),[left(1),right(4)]).

% Simple extract reverse and complement
extract2 <- create_fasta_file(F) | sequence::extract(file(F),[left(1),right(4)]).

% Extract everything
extract3 <- create_fasta_file(F) | sequence::extract(file(F),[left(1),right(10)]).

extract4 <- create_fasta_file(F) | sequence::extract(file(F),[left(8),right(2)]).

%%%% THE ACTUAL TESTCASES %%%%

testcase(match) :-
	run(matchfile),
	get_result_file(matchfile,File),
	file_exists(File),
	check_format(text(prolog(ranges(gene))),File).
	
run_check_and_get_seq(Goal,File,Seq) :-
	rerun(Goal),
	get_result_file(Goal,File),
	check_or_fail(file_exists(File),file_does_not_exist),
	check_or_fail(check_format(text(prolog(ranges(gene))),File),format_is_wrong),
	terms_from_file(File,[GeneTerm]),
	gene_extra_field(GeneTerm,sequence,Seq).

testcase(extract1) :-
	Expect = [a,g,c,t],
	run_check_and_get_seq(extract1,File,Seq),
	check_or_fail(Seq=Expect,not_equal(Seq,Expect)).
	
testcase(extract2) :-
	Expect = [a,g,c,t],	
	run_check_and_get_seq(extract2,File,Seq),
	check_or_fail(Seq=Expect,not_equal(Seq,Expect)).
	
testcase(extract3) :-
	Expect = [a,g,c,t,a,c,a,t,g,g],	
	run_check_and_get_seq(extract3,File,Seq),
	check_or_fail(Seq=Expect,not_equal(Seq,Expect)).

testcase(extract4) :-
	Expect = [g,g,a,g],
	run_check_and_get_seq(extract4,File,Seq),
	check_or_fail(Seq=Expect,not_equal(Seq,Expect)).
	
	