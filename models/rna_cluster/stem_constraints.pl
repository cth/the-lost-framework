% A DCG to check the stem length constraint
% e.g. the call
%	sequence_with_stem(4,['<','<','<','<','.','>','>','>','>'],[]).
% ensures that the sequence given as second argument has a stem of at least length 4.

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

	

