all_codons(L) :-
        L = [ [a,a,a],[a,a,t],[a,a,c],[a,a,g],
              [a,t,a],[a,t,t],[a,t,c],[a,t,g],
              [a,c,a],[a,c,t],[a,c,c],[a,c,g],
              [a,g,a],[a,g,t],[a,g,c],[a,g,g],
              [t,a,a],[t,a,t],[t,a,c],[t,a,g],
              [t,t,a],[t,t,t],[t,t,c],[t,t,g],
              [t,c,a],[t,c,t],[t,c,c],[t,c,g],
              [t,g,a],[t,g,t],[t,g,c],[t,g,g],
              [c,a,a],[c,a,t],[c,a,c],[c,a,g],
              [c,t,a],[c,t,t],[c,t,c],[c,t,g],
              [c,c,a],[c,c,t],[c,c,c],[c,c,g],
              [c,g,a],[c,g,t],[c,g,c],[c,g,g],
              [g,a,a],[g,a,t],[g,a,c],[g,a,g],
              [g,t,a],[g,t,t],[g,t,c],[g,t,g],
              [g,c,a],[g,c,t],[g,c,c],[g,c,g],
              [g,g,a],[g,g,t],[g,g,c],[g,g,g]].

facts :-
		all_codons(L),
		findall(codon(N,Codon),nth(N,L,Codon),Codons),
		forall(member(C,Codons),(write(C),write('.'),nl)).
		
start_codons(L) :-
	L = [[t,t,g],[c,t,g],[a,t,t],[a,t,c],[a,t,a],[a,t,g],[g,t,g]].
stop_codons(L) :-
	L = [[t,a,a],[t,g,a],[t,a,g]].
	
	1,5,6,7,8,24,40,56
