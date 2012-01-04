
fasta_to_facts(FastaFile,FactsFile) :-
	open(FastaFile,read,In),
	open(FactsFile,write,Out),
	process_fasta_file(1,In,Out),
	close(In),
	close(Out).
	
% Read the fasta header (first line)
read_fasta_header(In,[Next|Rest]) :-
	get_char(In,Next),
	Next \= '\n',
	!,
	read_fasta_header(In,Rest).
read_fasta_header(_,[]).


% We are done.
process_fasta_file(Pos,In,Out) :-
	peek_char(In,end_of_file),
	get_char(In,end_of_file),
	write(Out,base_max(Pos)).

% Line may be terminated by a newline which we ignore.
process_fasta_file(Pos,In,Out) :-
	peek_char(In,'\n'),
	get_char(In,'\n'),
	!,
	process_fasta_file(Pos,In,Out).
	
% Length of Window is 3:
process_fasta_file(Pos,In,Out) :-
	((0 is Pos mod 100000) ->
		write('Processed '), write(Pos),  writeln(' bases.'),
		statistics
		;
		true),
	get_char(In,NextChar),
	Pos1 is Pos + 1,
	writeq(Out,base(Pos,NextChar)),
	write(Out,'.\n'),
	!,
	process_fasta_file(Pos1,In,Out).