% This is the place for various predicates for manipulating
% biological sequences.

% reverse_complement(++NucleotideSequence,--ReverseComplementedNucleotideSequence)
dna_seq_reverse_complement([],[]).

dna_seq_reverse_complement([N|NucleotidesRest],[NC|NCRest]) :-
	dna_complement(N,NC),
	dna_seq_reverse_complement(NucleotidesRest,NCRest).

dna_complement(a,t).
dna_complement(t,a).
dna_complement(g,c).
dna_complement(c,g).


	