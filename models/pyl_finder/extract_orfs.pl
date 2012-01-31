:- dynamic base/3.

:- use(path).

/****************************************************************************************************
Options and constraints
*****************************************************************************************************/

min_orf_length(60).


/****************************************************************************************************
Main control of program
*****************************************************************************************************/

go :- extract_orfs1('longer.fasta').

extract_orfs1(FastaFile) :-
	codon_output_files(OutFiles),
	extract_starts_and_stops(FastaFile,OutFiles),
	load_genome(FastaFile,Genome),
	find_orfs(Genome,'codons+1.dat','orfs+1.dat','+',1),
	find_orfs(Genome,'codons+2.dat','orfs+2.dat','+',2),
	find_orfs(Genome,'codons+1.dat','orfs+3.dat','+',3),
	find_orfs(Genome,'codons-1.dat','orfs-1.dat','-',1),
	find_orfs(Genome,'codons-2.dat','orfs-2.dat','-',2),
	find_orfs(Genome,'codons-3.dat','orfs-3.dat','-',3),
	orfs_output_files(OrfOutputFiles),
	writeln('Merging files for all reading frames into orfs.pl'),
	merged_sorted_files(OrfOutputFiles,'orfs.pl'),
	writeln('Done.').

codon_output_files(['codons+1.dat', 'codons+2.dat','codons+3.dat', 'codons-1.dat', 'codons-2.dat', 'codons-3.dat']).
orfs_output_files(['orfs+1.dat', 'orfs+2.dat','orfs+3.dat', 'orfs-1.dat', 'orfs-2.dat', 'orfs-3.dat']).

extract_orfs(FastaFile,OutputFile) :-
	writeln(extract_orfs(FastaFile,OutputFile)),
	lost_tmp_file('pyl-finder-codons+1', CodonsDirect1),
	lost_tmp_file('pyl-finder-codons+2', CodonsDirect2),
	lost_tmp_file('pyl-finder-codons+3', CodonsDirect3),
	lost_tmp_file('pyl-finder-codons-1', CodonsReverse1),
	lost_tmp_file('pyl-finder-codons-2', CodonsReverse2),
	lost_tmp_file('pyl-finder-codons-3', CodonsReverse3),
	lost_tmp_file('pyl-finder-orfs+1', ORFSDirect1),
	lost_tmp_file('pyl-finder-orfs+2', ORFSDirect2),
	lost_tmp_file('pyl-finder-orfs+3', ORFSDirect3),
	lost_tmp_file('pyl-finder-orfs-1', ORFSReverse1),
	lost_tmp_file('pyl-finder-orfs-2', ORFSReverse2),
	lost_tmp_file('pyl-finder-orfs-3', ORFSReverse3),
	writeln(here),nl,
	extract_starts_and_stops(FastaFile,[CodonsDirect1,CodonsDirect2,CodonsDirect3,CodonsReverse1,CodonsReverse2,CodonsReverse3]),
	find_orfs(Genome,CodonsDirect1,ORFSDirect1,'+',1),
	find_orfs(Genome,CodonsDirect2,ORFSDirect2,'+',2),
	find_orfs(Genome,CodonsDirect3,ORFSDirect3,'+',3),
	find_orfs(Genome,CodonsReverse1,ORFSReverse1,'-',1),
	find_orfs(Genome,CodonsReverse2,ORFSReverse2,'-',2),
	find_orfs(Genome,CodonsReverse3,ORFSReverse3,'-',3),
	writeln('Merging files for all reading frames into orfs.pl'),
	merged_sorted_files([ORFSDirect1,ORFSDirect2,ORFSDirect3,ORFSReverse1,ORFSReverse2,ORFSReverse3],OutputFile),
	writeln('Done.').

/****************************************************************************************************
	Read a fasta file and produces a file (for each reading frame) with all stops and starts.
*****************************************************************************************************/

extract_starts_and_stops(File,OutputFiles) :-
	open(File,read,In),
	findall(OutStream, (member(OutFile,OutputFiles),open(OutFile,write,OutStream)),OutStreams),
	read_fasta_header(In,FirstLine), % Read the fasta header: Dont care...
	write('header: '), write(FirstLine),nl,
	write('Matching start and stop codons in file: '),write(File),nl,
	process_fasta_file(1,[],[],In,OutStreams),
	nl,
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
process_fasta_file(Pos,[],LastWindow,In,Out) :-
	peek_char(In,end_of_file),
	get_char(In,end_of_file),
	match_window(Pos,LastWindow,Out).

% If we reach the end of the file, then process the first window in circular fashion
process_fasta_file(Pos,FirstWindow,Window,In,Out) :-
	peek_char(In,end_of_file),
	Pos > 3,
	assert(max_pos(Pos)), % assert maximally observed position
	process_fasta_file(1,FirstWindow,Window,In,Out).
	
process_fasta_file(Pos,[F|FirstRest],[W|Window],In,Out) :-
	peek_char(In,end_of_file),
	match_window(Pos,[W|Window],Out),
	Pos1 is Pos + 1,
	append(Window,[F],NextWindow),
	process_fasta_file(Pos1,FirstRest,NextWindow,In,Out).

% Line may be terminated by a newline which we ignore.
process_fasta_file(Pos,FirstWindow,Window,In,Out) :-
	peek_char(In,'\n'),
	get_char(In,'\n'),
	!,
	process_fasta_file(Pos,FirstWindow,Window,In,Out).
	
% Length of Window is 3:
process_fasta_file(Pos,FirstWindow,[W|Window],In,Out) :-
	length(Window,2),
	((0 is Pos mod 20000) -> write('.') ; true),
	((0 is Pos mod 100000) -> write(Pos) ; true),
	match_window(Pos,[W|Window],Out),
	get_char(In,NextChar),
	append(Window,[NextChar],NextWindow),
	Pos1 is Pos + 1,
	!,
	process_fasta_file(Pos1,FirstWindow,NextWindow,In,Out).

% Length of window is less than three:
process_fasta_file(Pos,_,Window,In,Out) :-
	get_char(In,NextChar),
	append(Window,[NextChar],NextWindow),
	Pos1 is Pos + 1,
	!,
	process_fasta_file(Pos1,NextWindow,NextWindow,In,Out).

/********************************************************************************
** Matching of stop and start codons.

This is done on the fly while processing the fasta and creating the circular 
linked list. 

Matches are written to separate files (one for each reading fram), which are later
loaded when orfs are computed.

*********************************************************************************/


% match_window(+Pos,+Window,+Out):
% Pos is the position of the last element in the window
match_window(Pos,Codon,OutStreams) :-
	downcase(Codon,LowerCaseCodon),
	match_window_lowercase(Pos,LowerCaseCodon,OutStreams).
	
match_window(_,_,_). % Match everything else,

match_window_lowercase(Pos,Codon,OutStreams) :-
	((Pos - 3 > 0) ->
		StartWindow is Pos - 3
		;
		max_pos(MaxPos),
		StartWindow is MaxPos + (Pos-3)),
	start_codon(Codon),
	Index is Pos mod 3,
	nth0(Index,OutStreams,Out),
	write(Out,start(StartWindow)),
	writeln(Out,'.').
	
match_window_lowercase(Pos,Codon,OutStreams) :-
	((Pos - 3 > 0) ->
		StartWindow is Pos - 3
		;
		max_pos(MaxPos),
		StartWindow is MaxPos + (Pos-3)),
	stop_codon(Codon),
	Index is Pos mod 3,
	nth0(Index,OutStreams,Out),
	((Codon = [t,a,g]) ->
		write(Out,amber(StartWindow))
		;
		write(Out,stop(StartWindow))),		
	writeln(Out,'.').
	
match_window_lowercase(Pos,Codon,OutStreams) :-
	reverse_start_codon(Codon),
	Index is 3 + (Pos mod 3),
	nth0(Index,OutStreams,Out),
	((Pos - 1 > 0) ->
		Start is Pos - 1
		;
		max_pos(MaxPos),
		Start is MaxPos + (Pos - 1)),
	write(Out,start(Start)),
	writeln(Out,'.').

match_window_lowercase(Pos,Codon,OutStreams) :-
	reverse_stop_codon(Codon),
	Index is 3 + (Pos mod 3),
	nth0(Index,OutStreams,Out),
	((Pos - 1 > 0) ->
		Stop is Pos - 1
		;
		max_pos(MaxPos),
		Stop is MaxPos + (Pos - 1)),
	((Codon = [c,t,a]) -> % t,a,g reversed and complemented
		write(Out,amber(Stop))
		;
		write(Out,stop(Stop))),
	writeln(Out,'.').

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
start_codon([a,t,t]).
start_codon([a,t,c]).
start_codon([a,t,a]).
start_codon([a,t,g]).
start_codon([g,t,g]).

% Stop Codons:
stop_codon([t,a,a]).
stop_codon([t,a,g]). % amber codon (dont consider for now)
stop_codon([t,g,a]).

inframe_stop_codon([t,a,g]).

:- table downcase(+,-).

downcase([],[]).
downcase(['A'|Xs],[a|Ys]) :- !, downcase(Xs,Ys).
downcase(['G'|Xs],[g|Ys]) :- !, downcase(Xs,Ys).
downcase(['C'|Xs],[c|Ys]) :- !, downcase(Xs,Ys).
downcase(['T'|Xs],[t|Ys]) :- !, downcase(Xs,Ys).
downcase([X|Xs],[X|Ys]) :- downcase(Xs,Ys).

complement([],[]).
complement([X|Xs],[Y|Ys]) :-
	base_complement(X,Y),
	complement(Xs,Ys).
	
base_complement(a,t).
base_complement(t,a).
base_complement(c,g).
base_complement(g,c).

/***********************************************************
% Genome data structure
% Extract a range from the genome as a list
************************************************************/

load_genome(FastaFile,HashTable) :-
	retractall(base(_,_,_)),
	open(FastaFile,read,In),
	read_fasta_header(In,_),
	write('* Loading sequence from file: '), write(FastaFile), write(' into memory (circular array)'),
	new_hashtable(HashTable),
	load_next_base(1,In,HashTable),
	writeln('Done.'),
	close(In).

load_next_base(Pos,In,_HashTable) :-
	peek_char(In,end_of_file),
	get_char(In,end_of_file),
	PrevPos is Pos  - 1, 
	assert(base_max(PrevPos)).
%	retract(base(PrevPos,Base,Pos)),
%	assert(base(PrevPos,Base,1)).

load_next_base(Pos,In,HashTable) :-
	peek_char(In,'\n'),
	get_char(In,'\n'),
	!,
	load_next_base(Pos,In,HashTable).

load_next_base(Pos,In,HashTable) :-
	((0 is Pos mod 20000) -> write('.') ; true),
	((0 is Pos mod 100000) -> write(Pos) ; true),
	get_char(In,NextChar),
	Pos1 is Pos + 1,
	downcase([NextChar],[NextCharLowerCase]),
	hashtable_register(HashTable,Pos,NextCharLowerCase),
	assert(base(Pos,NextCharLowerCase,Pos1)),
	!,
	load_next_base(Pos1,In,HashTable).


get_range(StartEnd,StartEnd,HashTable,[Base]) :-
	hashtable_get(HashTable,StartEnd,Base).

get_range(Start,End,HashTable,[Base|Rest]) :-
	base_max(Start),
	hashtable_get(HashTable,Start,Base),
	!,
	get_range(1,End,HashTable,Rest).
	
get_range(Start,End,HashTable,[Base|Rest]) :-
	hashtable_get(HashTable,Start,Base),
	Next is Start + 1,
	!,
	get_range(Next,End,HashTable,Rest).

orf_length(LeftRight,LeftRight,1).
orf_length(Left,Right,Length) :-
	Left < Right,
	Length is 1 + Right - Left.
orf_length(Left,Right,Length) :-
	Left > Right,
	base_max(Last),
	Length is (1 + Last - Left) + Right.

increment_position(Pos,Add,NewPos) :-
	base_max(Max),
	Max >= Pos + Add,
	NewPos is Pos + Add.

increment_position(Pos,Add,NewPos) :-
	base_max(Max),
	Max < Pos + Add,
	NewPos is (Pos + Add) - Max.
	
decrement_position(Pos,Subtract,NewPos) :-
	0 >= Pos - Subtract,
	!,
	base_max(Max),
	NewPos is  Pos + Max - Subtract.

decrement_position(Pos,Subtract,NewPos) :-
	NewPos is Pos - Subtract.

/***********************************************************
% Genome data structure
% Extract a range from the genome as a list
************************************************************/

find_orfs(Genome,CodonFile,OrfFile,Strand,Frame) :-
	index_create(CodonFile),
	write('Computing open reading frames positions for strand '), write(Strand), write(' and frame '), write(Frame),nl,
	((Strand == '-') -> writeln('  - reversing index (for reverse strand lookups)'), index_reverse ; true),
	forward_orfs(Orfs),!,
	writeln('Sorting orfs in order of left position ascending'),nl,
	sort(Orfs,SortedOrfs),
	writeln('Done.'),
	retractall(ci(_,_,_,_)),!,
	writeln('Done.'),
	write('Writing orfs to file: '),
	writeln(OrfFile), 
	writeln('here'),
	open(OrfFile,write,OutStream),
	annotate_orfs(Strand,Frame,SortedOrfs,OutStream,Genome),
	write('after open'),
	write('after writing orfs'),
	close(OutStream),
	writeln('Done.').
	
annotate_orfs(_Strand,_Frame,[],_OutStream,_Genome) :- write('basecase.').

% Skip if the orf does have an inframe stop
annotate_orfs(Strand,Frame,[orf(_Stop,_Starts,[])|Rest],OutStream,Genome) :-
	!,
	annotate_orfs(Strand,Frame,Rest,OutStream,Genome).
		
annotate_orfs(Strand,Frame,[orf(Stop,Starts,InFrames)|Rest],OutStream,Genome) :-
%	write('matched orf: '),
%	writeln(orf(Stop,Starts,InFrames)),
	((Strand == '+') ->
		increment_position(Stop,2,Right), % i,e, Right is Stop + 2, if not across terminus
		Right is Stop + 2,
		Starts = [Left|_] % Left position coincides with first start
		;
		decrement_position(Stop,2,Left),
		Starts = [Right|_]
		),
	orf_length(Left,Right,OrfLength),
	min_orf_length(MinLength),
	((OrfLength >= MinLength) ->
		/*
		get_range(Left,Right,Genome,Sequence1),
		((Strand == '+') ->
			Sequence = Sequence1		
			;
			reverse(Sequence1,RevSeq),
			complement(RevSeq,Sequence)
		),
		*/
		write_orf(OutStream,Left,Right,Strand,Frame,OrfLength,Stop,Starts,InFrames)
		;
		true),
	!,
	annotate_orfs(Strand,Frame,Rest,OutStream,Genome).

write_orf(OutStream,Left,Right,Strand,Frame,Length,Stop,Starts,InFrames) :-
	writeq(OutStream,orf(na,Left,Right,Strand,Frame,[length(Length),stop(Stop),starts(Starts),in_frame_stops(InFrames)])),
	write(OutStream,'.\n').
	

index_create(File) :-
	open(File,read,InStream),!,
	writeln('Reading file into memory and creating indexes.'),!,
	create_circular_index(1,1,InStream),!,
	writeln('Done'),!,
	catch(close(InStream, [force(true)]),_,true).

create_circular_index(Index,FirstIndex,InStream) :-
	read(InStream,Term),
%	((0 is Index mod 10000) -> statistics ; true),	
	((Term == end_of_file) ->
		retract(ci(PrevIndex,Index,Type,Position)),
		assert(ci(PrevIndex,FirstIndex,Type,Position))
		;
		Term =.. [ Type, Position ],
		NextIndex is Index + 1,			
		assert(ci(Index,NextIndex,Type,Position)),
		!,
		create_circular_index(NextIndex,FirstIndex,InStream)).

index_reverse :- index_reverse(1,1).

% Base case: we circled all the way around.
index_reverse(Idx,NextIdx) :-
	ci(Idx,NextIdx,_,_),
	ci(NextIdx,_,NextType,NextPos),
	!,
	assert(ci_rev(NextIdx,Idx,NextType,NextPos)),
	retractall(ci(_,_,_,_)),
	forall(ci_rev(I1,I2,T,P),(retract(ci_rev(I1,I2,T,P)), assert(ci(I1,I2,T,P)))).

% Recursive case:	
index_reverse(Idx,FirstIndex) :-
	ci(Idx,NextIdx,_,_),
	ci(NextIdx,_,NextType,NextPos),
%	((0 is Idx mod 1000) -> write(index_reverse(Idx)),nl, statistics ; true),
	assert(ci_rev(NextIdx,Idx,NextType,NextPos)),
	!,
	index_reverse(NextIdx,FirstIndex).

/*
ci_search_(forward|backward) search (forward/backward) from a given position in the circular array
until it encounters an element of Type (e.g. a stop). It unifies the third argument with the position
of that stop.
*/

ci_search_forward(Type,StartIdx,MatchIdx) :-
	ci(StartIdx,NextIdx,_,_),
	ci_search_forward_rec(Type,NextIdx,MatchIdx).

ci_search_forward_rec(Type,StartIdx,StartIdx) :- 
	ci(StartIdx,_,Type,_).
ci_search_forward_rec(Type,StartIdx,StopIndex) :-
	ci(StartIdx,NextIdx,start,_),
	ci_search_forward_rec(Type,NextIdx,StopIndex).

ci_search_backward(Type,StartIdx,MatchIdx) :-
	ci(PrevIdx,StartIdx,_,_),
	ci_search_backward_rec(Type,PrevIdx,MatchIdx).

ci_search_backward_rec(Type,StartIdx,StartIdx) :-
	ci(StartIdx,_,Type,_).
ci_search_backward_rec(Type,StartIdx,MatchIdx) :-
	ci(PrevIdx,StartIdx,_,_),
	ci_search_backward_rec(Type,PrevIdx,MatchIdx).

forward_orfs(Orfs) :-
	ci_search_forward(stop,1,FirstStopIdx),   % Find first stop codon
	ci(FirstStopIdx,NextIdx,_,_),             % Let, NextIdx be the position after that
	forward_orfs_rec(FirstStopIdx,NextIdx,[],[],Orfs).

% Collect orfs on the direct strand.
% WE CIRCLED AROUND!
forward_orfs_rec(CurrentIdx,CurrentIdx,[],_,[]) :- !.

forward_orfs_rec(CurrentIdx,CurrentIdx,StartsInBetween, StopsInBetween, [orf(Position,Starts,InFrameStops)]) :-
	reverse(StartsInBetween,Starts),
	reverse(StopsInBetween,InFrameStops),
	ci(CurrentIdx,_,stop,Position).

% In the case where there are no preceding start codons, it is not an ORF, so just skip it.
forward_orfs_rec(StartIdx,CurrentIdx,[],_,RestOrfs) :-
	ci(CurrentIdx,NextIdx,stop,_),
	!,
	forward_orfs_rec(StartIdx,NextIdx,[],[],RestOrfs).
	
forward_orfs_rec(StartIdx,CurrentIdx,StartsInBetween,StopsInBetween,[orf(Position,Starts,InFrameStops)|RestOrfs]) :-
	ci(CurrentIdx,NextIdx,stop,Position),
	reverse(StartsInBetween,Starts),
	reverse(StopsInBetween,InFrameStops),
	!,
	forward_orfs_rec(StartIdx,NextIdx,[],[],RestOrfs).


forward_orfs_rec(StartIdx,CurrentIdx,StartsInBetween,StopsInBetween,RestOrfs) :-
	ci(CurrentIdx,NextIdx,start,Position),
	!,
	forward_orfs_rec(StartIdx,NextIdx,[Position|StartsInBetween],StopsInBetween,RestOrfs).

forward_orfs_rec(StartIdx,CurrentIdx,StartsInBetween,StopsInBetween,RestOrfs) :-
	ci(CurrentIdx,NextIdx,amber,Position),
	!,
	forward_orfs_rec(StartIdx,NextIdx,StartsInBetween,[Position|StopsInBetween],RestOrfs).


/***********************************************************
% Merged sorted files. 
% This merges files containing sorted prolog facts such that
% the merged file is also sorted.
************************************************************/

merged_sorted_files(InputFiles,OutputFile) :-
	findall(IFS,(member(IF,InputFiles),open(IF,read,IFS)),InStreams),
	open(OutputFile,write,OutStream),
	findall([InitialTerm,IS],(member(IS,InStreams),read(IS,InitialTerm)), InitialTerms),
	recursive_merge_files(OutStream,InitialTerms),
	forall(member(IS,InStreams),close(IS)),
	close(OutStream).

recursive_merge_files(_OutStream,[]).
	
recursive_merge_files(OutStream,Terms) :-
	sort(Terms,[[end_of_file,_]|SortedTerms]),
	!,
	recursive_merge_files(OutStream,SortedTerms).

recursive_merge_files(OutStream,Terms) :-
	sort(Terms,[[Term,InStream]|SortedTerms]),
	writeq(OutStream,Term),
	write(OutStream,'.\n'),
	read(InStream,NextTerm),
	!,
	recursive_merge_files(OutStream,[[NextTerm,InStream]|SortedTerms]).
		



