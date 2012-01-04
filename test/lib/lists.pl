:- use(lists).

% Should since not all elements of list are atoms
testcase(atom_concat_list) :-
	atom_concat_list([1,a,b,2], '1ab2').

testcase(intersperse) :-
	intersperse(a,[b,b,b],[b,a,b,a,b]).
	

square_it(X,Y) :- Y is X * X.
testcase(map(1)) :-
	map(square_it, [1,2,4],[1,4,16]).
	
testcase(take) :-
	take(2,[a,b,c,d],[a,b]).
	
testcase(split_list) :-
	split_list(2,[a,b,c,d],[a,b],[c,d]).
	
testcase(zip_1) :-
	zip([a,b,c,d],[1,2,3,4],[[a,1],[b,2],[c,3],[d,4]]).

testcase(inlists_nth0) :-
	inlists_nth0([[a,b],[a,b,c],[b,b]],1,[b,b,b]).

testcase(flexible_append) :-
	flexible_append(a,[b,c],[a,b,c]),
	flexible_append([a,b],c,[a,b,c]).
	
testcase(flatten_once) :-
	flatten_once([[a,b],[c,d],[e,f]],[a, b, c, d, e, f]),
	flatten_once([[a,[b,c]],[e,f]],[a,[b,c],e,f]).

testcase(rotate_list_vector) :-
 rotate_list_vector([[1,1,1],[2,2,2],[3,3,3]],[[1,2,3],[1,2,3],[1,2,3]]).

testcase(replace) :-
	replace(a,b,[a,b,a,b],[b,b,b,b]).
	
testcase(replace_list) :-
	replace_list([a,b],[b,a],[a,b,a,b],[b,a,b,a]).
	
testcase(not_member) :-
	not_member(a,[b,c,d]),
	not(not_member(a,[a,b,c])).
	

	

	

