% Program that reads a fasta file and produces a file with all stops

go :-
	match_stops('sequence.fasta').
	
match_stops(File ) :-
	open(File,read,In),
%	output_files(OutFiles),
%	findall(OutStream, (member(OutFile,OutFiles),open(OutFile,write,OutStream)),OutStreams),
%	writeln(OutStreams),
	read_fasta_header(In,FirstLine), % Read the fasta header: Dont care...
	write('read: '), write(FirstLine),nl,
	writeln('processing file...'),
	process_file(0,[],[],In,OutStreams),
	close(In),
%	forall(member(OutStr,OutStreams),close(OutStr)),
	true.

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


% match_window(+Pos,+Window,+Out):
% Pos is the position of the last element in the window

match_window(Pos,Codon,OutStreams) :-
	downcase(Codon,LowerCaseCodon),
	match_window_lowercase(Pos,LowerCaseCodon,OutStreams).
	
match_window(_,_,_). % Match everything else,

match_window_lowercase(Pos,Codon,OutStreams) :-
	StartWindow is Pos - 3,	
	start_codon(Codon),
	Index is Pos mod 3,
	nth0(Index,OutStreams,Out),	
	assert(start('+',Index,StartWindow)).
	
match_window_lowercase(Pos,Codon,OutStreams) :-
	StartWindow is Pos - 3,
	stop_codon(Codon),
	Index is Pos mod 3,
	nth0(Index,OutStreams,Out),
	assert(stop('+',Index,StartWindow)).
	
match_window_lowercase(Pos,Codon,OutStreams) :-
	reverse_start_codon(Codon),
	Index is 3 + (Pos mod 3),
	nth0(Index,OutStreams,Out),
	assert(start('-',Index,Pos)).

match_window_lowercase(Pos,Codon,OutStreams) :-
	reverse_stop_codon(Codon),
	Index is 3 + (Pos mod 3),
	nth0(Index,OutStreams,Out),	
	assert(stop('-',Index,Pos)).

:- table reverse_start_codon/1.
reverse_start_codon(ComplReverseCodon) :-
	start_codon(Codon),
	reverse(Codon,ReverseCodon),
	complement(ReverseCodon,ComplReverseCodon).

:- table reverse_stop_codon/1.
reverse_stop_codon(ComplReverseCodon) :-
	stop_codon(Codon),
	reverse(Codon,ReverseCodon),
	complement(ReverseCodon,ComplReverseCodon).

% Start codons:
start_codon([t,t,g]).
start_codon([c,t,g]).
start_codon([a,t,g]).
start_codon([a,t,g]).
start_codon([a,t,g]).
start_codon([a,t,g]).
start_codon([g,t,g]).

% Stop Codons:
stop_codon([t,g,a]).
stop_codon([t,a,a]).
stop_codon([t,a,g]).

:- table downcase(+,-).

downcase([],[]).
downcase(['A'|Xs],[a|Ys]) :- !, downcase(Xs,Ys).
downcase(['G'|Xs],[g|Ys]) :- !, downcase(Xs,Ys).
downcase(['C'|Xs],[c|Ys]) :- !, downcase(Xs,Ys).
downcase(['T'|Xs],[t|Ys]) :- !, downcase(Xs,Ys).
downcase([X|Xs],[X|Ys]) :- downcase(Xs,Ys).

:- table complement/2.

complement([],[]).
complement([X|Xs],[Y|Ys]) :-
	base_complement(X,Y),
	complement(Xs,Ys).
	
base_complement(a,t).
base_complement(t,a).
base_complement(c,g).
base_complement(g,c).

