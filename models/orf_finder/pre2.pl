%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Efficient genome data structure
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
data(blah,blah,blah,[a,g,c,t,t,t,t,c,a,t,t,c,t,g,a,c,t,g,c,a,a,c,g,g,g,c,a,a,
					t,a,t,g,t,c,t,c,t,g,t,g,t,g,g,a,t,t,a,a,a,a,a,a,a,g,a,g,t,
					g,t,c,t,g,a,t,a,g,c,a,g,c,t,t,c,t,g,a,a,c,t,g,g,t,t,a,c,c,
					t,g,c,c,g,t,g,a,g,t,a,a,a,t,t,a,a,a,a,t,t,t,t,a,t,t,g,a,c,
					t,t,a,g,g,t,c,a,c,t,a,a,a,t,a,c,t,t,t,a,a,c,c,a,a,t,a,t,a,
					g,g,c,a,t,a,g,c,g,c,a,c,a,g,a,c,a,g,a,t,a,a,a,a,a,t,t,a,c,
					a,g,a,g,t,a,c,a,c,a,a,c,a,t,c,c,a,t,g,a,a,a,c,g,c,a,t,t,a,
					g,c,a,c,c,a,c,c,a,t,t,a,c,c,a,c,c,a,c,c,a,t,c,a,c,c,a,t,t,
					a,c,c,a,c,a,g,g,t,a,a,c,g,g,t,g,c,g,g,g,c,t,g,a,c,g,c,g,t,
					a,c,a,g,g,a,a,a,c,a,c,a,g,a,a,a,a,a,a,g,c,c,c,g,c,a,c,c,t,
					g,a,c,a,g,t,g,c,g,g,g,c,t,t,t,t,t,t,t,t,t,c,g,a,c,c,a,a,a,
					g,g,t,a,a,c,g,a,g,g,t,a,a,c,a,a,c,c,a,t,g,c,g,a,g,t,g,t,t,
					g,a,a,g,t,t,c,g,g,c,g,g,t,a,c,a,t,c,a,g,t,g,g,c,a,a,a,t,g]).
*/

:- [big].

run :-
	data(_,_,_,List),
	length(List,L),
	writeln(L),
	List = [B1,B2|_],
	stop_codons(StopTriplets),
	start_codons(StartTriplets),	
	
%	triplet_match(1,[[start,StartTriplets],[stop,StopTriplets]],[B1,B2],List).
	triplet_match(1,[[stop,StopTriplets]],[B1,B2],List).

start_codons([[t,t,g],[c,t,g],[a,t,t],[a,t,c],[a,t,a],[a,t,g],[g,t,g]]).

stop_codons([[t,g,a],[t,a,a],[t,a,g]]).
	
triplet_match(_,_,_,[]).
	
triplet_match(P,Matchers,[B2,B3],[B1]) :-
	member([MatchId,Triplets],Matchers),
	member([B1,B2,B3],Triplets),
	AssertGoal =.. [ MatchId, P ],
	assert(AssertGoal).
	
triplet_match(P,Matchers,[B3,B4],[B1,B2]) :-
	member([MatchId,Triplets],Matchers),
	member([B1,B2,B3],Triplets),
	P1 is P + 1,
	AssertGoal =.. [ MatchId, P ],
	assert(AssertGoal),
	!,
	triplet_match(P1,Matchers,[B3,B4],[B2]).
		
triplet_match(P,Matchers,Init,[B1,B2,B3|Rest]) :-
	member([MatchId,Triplets],Matchers),
	member([B1,B2,B3],Triplets),
	P1 is P + 1,
	AssertGoal =.. [ MatchId, P ],
	assert(AssertGoal),
	!,
	triplet_match(P1,Matchers,Init,[B2,B3|Rest]).
	
triplet_match(P,Matchers,Init,[_|Bs]) :-
	P1 is P + 1,
	!,
	triplet_match(P1,Matchers,Init,Bs).