sequence([c,t,t,c,t,g,c,g,t,g,g,c,c,t,t,c,g,a,g,c,g,a,t,a,c,a,t,c,a,t,c,a,c,t,g,c,c,a,g,g,a,t,c,a,c,g,c,t,a,a,t,a,c,c,a,a,t,c,g,c,a,a,t,a,c,c,g,g,c,a,a,a,g,a,a,c,c,a,a,c,g,c,t,g,c,c,a,g,c,c,g,c,c,g,c,t,a,t,c,g,g,c,a,a,g,g,a,a,a,c,t,a,a,a,c,g,c,c,g,c,g,c,c,a,t,a,g,t,t,a,c,g,c,g,c,a,t,a,a,t,g,c,a,g,a,t,t,a,a,g,c,g,a,c,g,g,g,a,a,c,a,g,c,g,g,g,a,c,c,g,t,a,t,c,c,c,c,c,a,g,a,g,c,a,a,a,g,t,t,c,t,g,g,a,g,g,a,t,c,a,g,g,t,a,t,t,t,g,c,t,g,c,c,c,a,g,a,t,c,g,a,t,a,a,t,c,a,g,c,a,c,g,a,c,t,a,c,c,a,c,c,a,g,c,c,a,c,a,g,c,c,a,g,c,g,t,a,g]).
%starts([25488,25470,25410,25395,25368,25356,25353,25347,25320,25302,25275]),stop([25236])]).

mini_sequence([a,a,a,t,t,g,a,a,a]).

annotate_one_start(Sequence,Annotated) :-
	seq_triplets(Sequence,Triplets),
	start_codons(StartCodons),
	member(StartCodon,StartCodons),
	append(Part1,Part2,Triplets),
	append(BeforeStart,[StartCodon],Part1),
	append(BeforeStart,[start(StartCodon)],AnnotatedPart1),
	append(AnnotatedPart1,Part2,Annotated).

start_codons(L) :-
        L = [[t,t,g],[c,t,g],[a,t,t],[a,t,c],[a,t,a],[a,t,g],[g,t,g]].	
	
seq_triplets([],[]).
seq_triplets([N1,N2,N3|Ns],[[N1,N2,N3]|Rest]) :-
        seq_triplets(Ns,Rest).

test(OneStart) :-
	mini_sequence(S),
	annotate_one_start(S,OneStart).
	
btest(OneStart) :-
	sequence(S),
	annotate_one_start(S,OneStart).	

t :-
	append(Part1,Part2,[[a,a,a],[t,t,g],[a,a,a]]),
	append(BeforeStart,[[t,t,g]],Part1).