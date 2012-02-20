
:- table matrix/3.

:- dynamic target/2, align/3.

build_phylip_matrix(AligmentsFile,OutputFile) :-
	write(open(AligmentsFile,read,InStream)),
	open(AligmentsFile,read,InStream),	
	make_unique_table(InStream,1,NumEntries,0,MaxCost),
	assert(max_cost(MaxCost)),
	close(InStream),
	foreach(I in 1..NumEntries, assert(align(I,I,0))), % Self-alignments
	tell(OutputFile),
	write('\t'),write(NumEntries),nl,
	foreach(I in 1..NumEntries, phylip_line(I,NumEntries)),
	told.
	
make_unique_table(InStream,Idx,FinalIdx,MaxCostAcc,MaxCost) :-
	read(InStream,Term),
	((Term == end_of_file) ->
		FinalIdx is Idx - 1,
		MaxCost = MaxCostAcc
		;
		Term = [ Cost, A, B ],
		add_to_table(A,IdxA,Idx,Idx1),
		add_to_table(B,IdxB,Idx1,Idx2),
		assert(align(IdxA,IdxB,Cost)),
		((Cost > MaxCostAcc) ->
			MaxCostAcc1 = Cost
			;
			MaxCostAcc1 = MaxCostAcc),
		make_unique_table(InStream,Idx2,FinalIdx,MaxCostAcc1,MaxCost)
	).

add_to_table(A,IdxA,Idx,Idx) :-
	target(A,IdxA), !.
		
add_to_table(A,Idx,Idx,NextIdx) :-
	assert(target(A,Idx)),
	NextIdx is Idx + 1.
	
align_entry(A,B,C) :-
	align(A,B,C)
	;
	align(B,A,C).
	
phylip_line(I,NumEntries) :-
	target(Target,I),
	nodeid(Target,NameCodes,[]),
	atom_codes(Name,NameCodes),
	write(Name),
	write('\t'),
	foreach(J in 1..NumEntries,write_alignment_cost(I,J)),
	nl.

nodeid((Organism,LeftPos,RightPos)) -->
	{
		flatname(Organism,Flat),
		atom_codes(Flat,OrganismCodes)
	},
	OrganismCodes,
	"_",
	str(LeftPos),
	"-",
	str(RightPos).

str(Int) -->
	{
		number(Int),
		number_chars(Int,Chars),
		to_atom_codes(Chars,Codes)
	},
	Codes.

to_atom_codes([],[]).
to_atom_codes([C|Cs], [D|Ds]) :- atom_codes(C,[D]), to_atom_codes(Cs,Ds).
	
flatname(Target,Name) :-
	term2atom(Target,TAtom),
	atom_codes(TAtom,TCodes),
	atom_codes('\'(), ',RemoveCodes),
	delete_many(TCodes,RemoveCodes,CleanCodes),
%	atom_codes(',_',[Comma,Underscore]),
%	findall(X,(member(T,TCodes), ((T==Comma) -> X=Underscore ; X=T)),ReplaceCodes),
	atom_codes(Name,CleanCodes).
	
delete_many(List,[],List).
delete_many(ListIn,[X|Xs],ListOut) :-
	delete(ListIn,X,ListOut1),
	delete_many(ListOut1,Xs,ListOut).
	
write_alignment_cost(I,J) :-
	(align_entry(I,J,Cost) ->
		true
		;
		max_cost(Cost)),
/*		listing(align(_,_)),
		throw(no_align(I,J))), */
	write(Cost),
	write(' ').
	
test :-
	retractall(target(_,_)),
	retractall(align(_,_,_)),
	build_phylip_matrix('alignments.pl',whatever).