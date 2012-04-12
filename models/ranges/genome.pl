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