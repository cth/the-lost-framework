:- use(arithmetic).

testcase(abs) :-
	abs(-1, 1).
	abs(1, 1),
	abs(0, 0).

testcase(min) :-
	min(1,2,1).

testcase(max) :-
	max(1,2,2).
	
testcase(list_min) :-
	list_min([1,4,2,1,6,2],1),
	list_min([1,4,-2,1,6,2],-2).

testcase(list_max) :-
	list_max([4,1,45,23,2],45).
	