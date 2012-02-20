:- use(fasta).
:- use(lists).

clustalw_align(A,B,Distance) :-
	clean_tmp,
	gene_extra_field(A,pylis,SeqA),
	gene_extra_field(B,pylis,SeqB),
	create_input_file(SeqA,SeqB,InputFile),
	lost_tmp_file('clustalw_output',OutputFile),
	file_base_name(InputFile,InputFileBase),
	atom_concat_list([clustalw2,' "', InputFileBase,'" > "',OutputFile,'"'],ClustalWCommand),
%	writeln(ClustalWCommand),nl,
	working_directory(CurrentDirectory),
	lost_tmp_directory(TmpDir),
	chdir(TmpDir),
	system(ClustalWCommand),
	chdir(CurrentDirectory),
	readFile(OutputFile,OutputFileCodes),
	check_or_fail(parse_clustalw_output(AlignmentScore,OutputFileCodes,[]),
		failed_to_parse_clustalw_output_file(OutputFile)),
	Distance is 1 / AlignmentScore.
	
	
create_input_file(SeqA,SeqB,InputFile) :-
	to_fasta('a', SeqA, FastaA),
	to_fasta('b',SeqB, FastaB),
	lost_tmp_file('clustalw_input','.fasta',InputFile),
	append(FastaA,FastaB,Fasta),
	open(InputFile,write,Stream),
	forall(member(Code,Fasta),put_code(Stream,Code)),
	close(Stream).

parse_clustalw_output(Score) -->
	something,
	"Sequences (1:2) Aligned. Score:  ",
	integer(Score),
	"\n",
	something.

integer(Integer) -->
	digits(Digits),
	{
		Digits \= [],
		atom_codes(Atom,Digits),
		atom_integer(Atom,Integer)
	}.
	
digits([]) --> [].
digits([D|Ds]) --> digit(D), digits(Ds).

digit(D) --> [D], { atom_codes('0123456789',Digits), member(D,Digits) }.


something --> [].
something --> [_], something.

test_parse :-
	File = '/Users/cth/code/bitbucket/lost//tmp//clustalw_output-2787',
	readFile(File,Codes),
	parse_clustalw_output(Score,Codes,[]),
	writeln(Score).
