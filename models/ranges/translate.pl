:- use(genecode).
:- use(genedb).

translate_terms(_SequenceFunctor,_GeneCode,[],[]).

translate_terms(InStream,OutStream,SequenceFunctor,GeneCode) :-
	write('.'),
	read(InStream,Term),
	((Term == end_of_file) ->
		true
		;
		gene_extra_field(Term,SequenceFunctor,Sequence),
		translate(GeneCode,Sequence,ProteinSequence),
		gene_add_extra_field(Term,protein_sequence,ProteinSequence,TermProtein),
		writeq(OutStream,TermProtein),
		write(OutStream,'.\n'),
		!,
		translate_terms(InStream,OutStream,SequenceFunctor,GeneCode)).

%% translate(+GeneCode,+List_Nucleotides,-List_AA)
translate(_GeneCode,[],[]) :-
        !.

translate(_GeneCode,[_],[]) :-
        !.

translate(_GeneCode,[_,_],[]) :-
        !.

translate(GeneCode,[N1,N2,N3|RestOrf],[A|RestAs]):-
	genecode(GeneCode,[N1,N2,N3],A),
	translate(GeneCode,RestOrf,RestAs).