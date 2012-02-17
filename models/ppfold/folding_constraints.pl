/*
 filter a stream of gene terms according to given constraints.
*/

:- use(genedb).

read_and_filter_terms(InStream,OutStream,TermsRead) :-
	read(InStream,Term),
	((Term == end_of_file) ->
		nl
		;
		((0 is TermsRead mod 100) -> write('.') ; true),
		((0 is TermsRead mod 1000) -> write(TermsRead) ; true),
		(apply_sequence_constraints(Term) ->
			write('keeping one..'),nl,		
			writeq(OutStream,Term),
			write(OutStream,'.\n')
			;
			write('skipping one..'),nl
		),
		TermsRead1 is TermsRead + 1,
		!,
		read_and_filter_terms(InStream,OutStream,TermsRead1)).
		
apply_sequence_constraints(GeneTerm) :-
	gene_extra_field(GeneTerm,folding,Folding),
	gene_extra_field(GeneTerm,energy,Energy),
	% Checking min energy of folding
	min_energy(MinEnergy),
	Energy >= MinEnergy,
	% Counting base pairs:
	min_base_pairs(MinBasePairs),
	subtract(Folding,['.'],PairedBases),
	length(PairedBases,NumPairedBases),
	BasePairs is NumPairedBases // 2,
	BasePairs >= MinBasePairs,
	% Checking minimum stem length in folding
	min_stem_length(MinStemLength),
	write('here'),
	writeln(GeneTerm),
	sequence_with_stem(MinStemLength,Folding,[]).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% A DCG to check the stem length constraint
% e.g. the call
%	sequence_with_stem(4,['<','<','<','<','.','>','>','>','>'],[]).
% ensures that the sequence given as second argument has a stem of at least length 4.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sequence_with_stem(0) -->
	{!},
	any_sequence.

sequence_with_stem(Length) -->
	any_sequence,
	stem_left(Length),
	balanced_sequence,
	stem_right(Length),
	any_sequence.

stem_left(1) --> ['<'].
stem_left(X) --> { X>1,Xs is X - 1 }, stem_left(1), stem_left(Xs).

stem_right(1) --> ['>'].
stem_right(X) --> { X>1,Xs is X - 1 }, stem_right(1), stem_right(Xs).


any_sequence --> 
	sequence(_,_).

balanced_sequence --> 
	sequence(Same,Same).

sequence(0,0) --> [].
sequence(Left,Right) -->
	sequence_one(Left1,Right1),
	sequence(LeftRest,RightRest),
	{ 
		Left is Left1 + LeftRest,
		Right is Right1 + RightRest
	}.

sequence_one(0,0) --> ['.'].
sequence_one(1,0) --> ['<'].
sequence_one(0,1) --> ['>'].


test1 :-
	sequence_with_stem(4,['<','<','<','<','.','>','>','>','>'],[]).
test2 :-
	sequence_with_stem(4,['.','<','<','<','<','<','<','<','<','<','<','.','.','.','.','.','.','.','.','.','.','.','.','.','.','.','.','.','.','.','.','.','.','.','.','.','.','.','.','.','.','.','.','.','.','.','.','.','.','.','.','.','.','.','.','.','.','.','.','.','.','.','.','.','.','.','.','.','.','.','.','.','.','.','.','.','.','.','.','.','.','.','.','.','.','.','.','>','>','>','>','>','>','>','>','>','>','.','.','.'],[]).

test3 :-
	sequence_with_stem(4,['.','.','.','.','.','.','.','.','<','<','<','<','.','.','.','.','.','>','>','>','>','.','.','.','.','.','.','<','<','<','<','<','<','<','.','.','.','.','.','.','.','.','.','.','.','.','.','.','.','.','.','.','.','.','.','.','.','.','<','<','<','<','<','<','.','.','.','.','.','.','.','.','.','.','.','.','.','.','.','.','>','>','>','>','>','>','.','.','.','.','.','.','>','>','>','>','>','>','>','.'],[]).
test4 :-
	sequence_with_stem(0,['.','.','.','.','.','.','.','.'],[]).
