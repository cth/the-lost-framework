% Program that reads a fasta file and produces a file with all stops

go :-
	match_stops('sequence.fasta').
	
output_files(['direct-1.tmp', 'direct-2.tmp','direct-3.tmp', 'reverse-1.tmp', 'reverse-2.tmp', 'reverse-3.tmp']).

match_stops(File ) :-
	open(File,read,In),
	output_files(OutFiles),
	findall(OutStream, (member(OutFile,OutFiles),open(OutFile,write,OutStream)),OutStreams),
	writeln(OutStreams),
	read_fasta_header(In,FirstLine), % Read the fasta header: Dont care...
	write('read: '), write(FirstLine),nl,
	writeln('processing file...'),
	process_file(0,[],[],In,OutStreams),
	close(In),
	forall(member(OutStr,OutStreams),close(OutStr)).

% Read the fasta header (first line)
read_fasta_header(In,[Next|Rest]) :-
	get_char(In,Next),
	Next \= '\n',
	!,
	read_fasta_header(In,Rest).
read_fasta_header(_,[]).

% We are done.
process_file(Pos,[],LastWindow,In,Out) :-
	peek_char(In,end_of_file),
	get_char(In,end_of_file),
%	writeln(process_file0(Pos,[],LastWindow,In,Out)),	
	match_window(Pos,LastWindow,Out).


% If we reach the end of the file, then process the first window in circular fashion
process_file(Pos,[F|FirstRest],[W|Window],In,Out) :-
	peek_char(In,end_of_file),
%	writeln(process_file1(Pos,[F|FirstRest],[W|Window],In,Out)),
	match_window(Pos,[W|Window],Out),
	Pos1 is Pos + 1,
	append(Window,[F],NextWindow),
	process_file(Pos1,FirstRest,NextWindow,In,Out).

% Line may be terminated by a newline which we ignore.
process_file(Pos,FirstWindow,Window,In,Out) :-
	peek_char(In,'\n'),
	get_char(In,'\n'),
%	writeln(process_file2(Pos,FirstWindow,Window,In,Out)),
	!,
	process_file(Pos,FirstWindow,Window,In,Out).
	
% Length of Window is 3:
process_file(Pos,FirstWindow,[W|Window],In,Out) :-
	length(Window,2),
	((0 is Pos mod 100000) ->
		write('Processed '), write(Pos),  writeln(' bases.'),
		statistics
		;
		true),
%	writeln(process_file3(Pos,[W|Window],In,Out)),
	match_window(Pos,[W|Window],Out),
	get_char(In,NextChar),
	append(Window,[NextChar],NextWindow),
	Pos1 is Pos + 1,
	assert(f(Pos,NextChar)),
	!,
	process_file(Pos1,FirstWindow,NextWindow,In,Out).

% Length of window is less than three:
process_file(Pos,_,Window,In,Out) :-
%	writeln(process_file4(Pos,FirstWindow,Window,In,Out)),
	get_char(In,NextChar),
	append(Window,[NextChar],NextWindow),
	Pos1 is Pos + 1,
	!,
	process_file(Pos1,NextWindow,NextWindow,In,Out).

match_window(_,_,_).