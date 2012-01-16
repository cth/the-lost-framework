/*

Optimization ideas:

Assumptions:
Match sequences is of same size!!!
*/ 



go :-
	findall(Codon,start_codon(Codon),StartCodons),
	match_sequences(StartCodons,'longer.fasta','outfile.pl').
	
/****************************************************************************************************
	Read a fasta file and produces a file (for each reading frame) with all stops and starts.
*****************************************************************************************************/
match_sequences(MatchSequences,File,OutputFile) :-
	member(SomeSeq,MatchSequences),
	length(SomeSeq,WindowLength),
	open(File,read,In),
%	findall(OutStream, (member(OutFile,OutputFiles),open(OutFile,write,OutStream)),OutStreams),
	open(OutputFile,write,OutStream),
	read_fasta_header(In,FirstLine), % Read the fasta header: Dont care...
	write('header: '), write(FirstLine),nl,
	write('Matching sequences in file file: '),write(File),nl,
	process_fasta_file(1,0,WindowLength,0,MatchSequences,[],[],In,OutStream),
	close(In),
	close(OutStream).
%	forall(member(OutStr,OutStreams),close(OutStr)).

% Read the fasta header (first line)
read_fasta_header(In,[Next|Rest]) :-
	get_char(In,Next),
	Next \= '\n',
	!,
	read_fasta_header(In,Rest).
read_fasta_header(_,[]).

% We are done.
process_fasta_file(_LeftPos,_RightPos,_,_,_MatchSeqs,[],_LastWindow,In,_Out) :-
	peek_char(In,end_of_file),
	writeln('end_of_file'),
	get_char(In,end_of_file).

% If we reach the end of the file, then process the first window in circular fashion

% This case is only hit once:
% If right pos is larger than the window size then we know that this is 
process_fasta_file(LeftPos,RightPos,WindowSize,WindowSize,MatchSeqs,[F|FirstRest],[W|Window],In,Out) :-
	peek_char(In,end_of_file),
	RightPos > WindowSize, 
	!,
	match_window(LeftPos,RightPos,MatchSeqs,[W|Window],Out),
	append(Window,[F],NextWindow),
	LeftPos1 is LeftPos+1,	
	write_process_fasta_file(LeftPos1,1,WindowSize,WindowSize,MatchSeqs,FirstRest,NextWindow,In,Out).
	
process_fasta_file(LeftPos,RightPos,WindowSize,WindowSize,MatchSeqs,[F|FirstRest],[W|Window],In,Out) :-
	peek_char(In,end_of_file),
	match_window(LeftPos,RightPos,MatchSeqs,[W|Window],Out),
	LeftPos1 is LeftPos + 1,
	RightPos1 is RightPos + 1,
	append(Window,[F],NextWindow),
	!,
	write_process_fasta_file(LeftPos1,RightPos1,WindowSize,WindowSize,MatchSeqs,FirstRest,NextWindow,In,Out).

% Line may be terminated by a newline which we ignore.
process_fasta_file(LeftPos,RightPos,WindowSize,CurrentWindSize,MatchSeqs,FirstWindow,Window,In,Out) :-
	peek_char(In,'\n'),!,
	get_char(In,'\n'),
	!,
	write_process_fasta_file(LeftPos,RightPos,WindowSize,CurrentWindSize,MatchSeqs,FirstWindow,Window,In,Out).
	
% Length of Window is 3:
process_fasta_file(LeftPos,RightPos,WindowSize,WindowSize,MatchSeqs,FirstWindow,[W|Window],In,Out) :-
	((0 is RightPos mod 20000) -> write('.') ; true),
	((0 is RightPos mod 100000) -> write(RightPos) ; true),
	match_window(LeftPos,RightPos,MatchSeqs,[W|Window],Out),
	get_char(In,NextChar),
	downcase(NextChar,NextCharLower),
	append(Window,[NextCharLower],NextWindow),
	LeftPos1 is LeftPos + 1,
	RightPos1 is RightPos + 1,
	!,
	write_process_fasta_file(LeftPos1,RightPos1,WindowSize,WindowSize,MatchSeqs,FirstWindow,NextWindow,In,Out).

% Length of window is less than WindowSize:
process_fasta_file(LeftPos,RightPos,WindowSize,CurrentWindowSize,MatchSeqs,_,Window,In,Out) :-
	WindowSize > CurrentWindowSize,
	get_char(In,NextChar),
	downcase(NextChar,NextCharLower),
	append(Window,[NextCharLower],NextWindow),
	NextWindowSize is CurrentWindowSize + 1,
	RightPos1 is RightPos + 1,
	!,
	write_process_fasta_file(LeftPos,RightPos1,WindowSize,NextWindowSize,MatchSeqs,NextWindow,NextWindow,In,Out).
	
write_process_fasta_file(LeftPos,RightPos,WindowSize,CurrentWindowSize,MatchSeqs,FirstWindow,Window,In,Out) :-
	%writeln(process_fasta_file(LeftPos,RightPos,WindowSize,CurrentWindowSize,MatchSeqs,FirstWindow,Window,In,Out)),	
	process_fasta_file(LeftPos,RightPos,WindowSize,CurrentWindowSize,MatchSeqs,FirstWindow,Window,In,Out).
	
downcase('A',a).
downcase('G',g).
downcase('C',c).
downcase('T',t).
dowcase(X,X). % we dont care about anything else

/*match_window(_Pos,[a,a,a,a,a,a],_Out) :-
		write('+'), writeln([a,a,a,a,a,a]).
*/
%match_window(LeftPos,RightPos,Window,_Out) :- write(LeftPos), write('-'), write(RightPos), write(' : '), writeln(Window).
		
% match_window(+Pos,+Window,+Out):
% Pos is the position of the last element in the window

match_window(LeftPos,RightPos,MatchSequences,Window,Out) :-
	member(MatchSequence,MatchSequences),
	match_window_direct(LeftPos,RightPos,MatchSequence,Window,Out).

match_window(LeftPos,RightPos,MatchSequences,Window,Out) :-
	member(MatchSequence,MatchSequences),
	reverse_complement(MatchSequence,RevComplMatch),
	match_window_reverse(LeftPos,RightPos,MatchSequence,RevComplMatch,Window,Out).

match_window(_,_,_,_,_). % Match everything else, report nothing.

% Direct match
match_window_direct(LeftPos,RightPos,Match,Match,Out) :-
	ReadingFrame is 1 + LeftPos mod 3,
	write(Out,seq('na',LeftPos,RightPos,'+',ReadingFrame,[match(Match)])),
	writeln(Out,'.').
	
match_window_reverse(LeftPos,RightPos,OrigMatch,RevComplMatch,RevComplMatch,Out) :-
	ReadingFrame is 1 + LeftPos mod 3,
	write(Out,seq('na',LeftPos,RightPos,'-',ReadingFrame,[match(OrigMatch)])),
	writeln(Out,'.').

:- table reverse_complement/2.

reverse_complement(Seq,RevComplSeq) :-
	reverse(Seq,RevSeq),
	complement(RevSeq,RevComplSeq).
	
complement([],[]).
complement([X|Xs],[Y|Ys]) :-
	base_complement(X,Y),
	!,
	complement(Xs,Ys).
	
base_complement(a,t).
base_complement(t,a).
base_complement(c,g).
base_complement(g,c).