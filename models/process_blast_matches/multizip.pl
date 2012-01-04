% Multi-zip:
merge_multiple_lines(Lines,[]) :-
	forall(member(L,Lines), L==[]). % All lines are empty
	
merge_multiple_lines(Lines,[Elems|ElemsRest]) :-
	findall(Elem1,(member(Line,Lines),Line=[Elem1|_]), Elems),
	findall(LineRest,(member(Line,Lines),Line=[_|LineRest]), LinesRest),
	merge_multiple_lines(LinesRest,ElemsRest).


	% Basically, Multi-zip:
	sum_multiple_lines(Lines,[]) :-
		forall(member(L,Lines), L==[]). % All lines are empty

	sum_multiple_lines(Lines,[Sum|ElemsRest]) :-
		findall(Elem1,(member(Line,Lines),Line=[Elem1|_]), Elems),
		sumlist(Elems,Sum),
		findall(LineRest,(member(Line,Lines),Line=[_|LineRest]), LinesRest),
		sum_multiple_lines(LinesRest,ElemsRest).

test :-
	L1 = [1,0,1,0],
	L2 = [2,0,2,0],
	L3 = [3,3,0,0],
	merge_multiple_lines([L1,L2,L3],MergeResult),
	write(MergeResult).
	
	
	test2 :-
	L1 = [1,0,1,0],
	L2 = [2,0,2,0],
	L3 = [3,3,0,0],
	sum_multiple_lines([L1,L2,L3],MergeResult),
	write(MergeResult).