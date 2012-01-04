/************************************************************************************************************************
Efficient genome data structure:
This loads a fasta sequence file into memory structured as a circular linked list.
 
Using the get_range/3 predicate it is then possible to efficiently access the list and extract a portion of it,
possible spanning over the terminus.
************************************************************************************************************************/

load_genome(FastaFile) :-
	data(_,_,_,List),
	open(FastaFile,read,InStream),
	read_fasta_header(_),
	create_genome_linked_list(InStream),
	close(InStream).

% Read the fasta header (first line)
read_fasta_header(In,[Next|Rest]) :-
	get_char(In,Next),
	Next \= '\n',
	!,
	read_fasta_header(In,Rest).
read_fasta_header(_,[]).


create_genome_index(InStream) :-
	assert($base_min(1)),
	assert_genome_terms(1,InStream).

assert_genome_terms(_,[]).

assert_genome_terms(N,[L]) :-
	!,
	assert($base_max(N)),
	assert($base(N,L,1)).

assert_genome_terms(N,InStream) :-
	read(InStream,Term),
	((Term == end_of_file) ->
		true
		;
		N1 is N + 1,
		assert($base(N,L,N1)),		
	)
	N1 is N + 1,
	assert($base(N,L,N1)),
	!,
	assert_genome_terms(N1,Ls).


% Ensure efficient reverse lookups:
base(Pos,Base,PosNext) :-
	var(Pos),
	ground(PosNext),
	!,
	$base_min(Min),
	$base_max(Max),
	((PosNext == Min) ->
		$base(Max,Base,PosNext)
		;
		PrevPos is PosNext - 1,
		$base(PrevPos,Base,PosNext)).
		
base(Pos,Base,PosNext) :-
	$base(Pos,Base,PosNext).



	
create_genome_index(List) :-
	assert($base_min(1)),
	assert_genome_terms(1,List).
	

% Extract a range from the genome as a list
get_range(StartEnd,StartEnd,[Base]) :-
	base(StartEnd,Base,_).

get_range(Start,End,[Base|Rest]) :-
	base(Start,Base,Next),
	!,
	get_range(Next,End,Rest).
	
match_with_list_direct(_,[]).
match_with_list_direct(Position,[M|Ms]) :-
	base(Position,M,NextPos),
	!,
	match_with_list_direct(NextPos,Ms).

match_with_list_reverse(Pos,List) :-
	reverse(List,ReverseList),
	base(Pos,_,NextPos),
	!,
	match_with_list_reverse_rec(NextPos,ReverseList).
	
match_with_list_reverse_rec(_,[]).
match_with_list_reverse_rec(NextPos,[M|Ms]) :-
	compl(M,C),
	base(Pos,C,NextPos),
	!,
	match_with_list_reverse_rec(Pos,Ms).
	
match_with_lists_direct(Position,Lists) :-
	member(List,Lists),
	match_with_list_direct(Position,List).
	
match_with_lists_reverse(Position,Lists) :-
	member(List,Lists),
	match_with_list_reverse(Position,List).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Extract stops lists
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

stop_codons([[t,g,a],[t,a,a],[t,a,g]]).

stops_direct(Stops) :-
	$base_min(Min),
	$base_max(Max),
	match_stops_direct(Min,Max,Stops).

match_stops_direct(StartEnd,StartEnd,[]).

match_stops_direct(Pos,End,[Pos|StopsRest]) :-
	stop_codons(StopCodons), % Fixme - 
	match_with_lists_direct(Pos,StopCodons),
	!,
	write(Pos),nl,
	base(Pos,_,NextPos),
	!,
	match_stops_direct(NextPos,End,StopsRest).

match_stops_direct(Pos,End,StopsRest) :- 
	base(Pos,_,NextPos),
	!,
	match_stops_direct(NextPos,End,StopsRest).
	
match_stops_reverse(StartEnd,StartEnd,[]).

match_stops_reverse(Start,Start,[]) :-
	stop_codons(StopCodons)
