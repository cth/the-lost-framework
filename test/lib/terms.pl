testcase(atom_integer_1) :-
	atom_integer('123',X),
	X = 123.
		
testcase(atom_integer_2) :-
	atom_integer(X,123),
	X = '123'.
	
testcase(term_to_atom) :-
	term_to_atom(a(b(c,d(1,f))), 'a(b(c,d(1,f)))').