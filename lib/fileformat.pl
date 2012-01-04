:- use(prologdb).
:- use(genedb).

check_format(text(prolog(ranges(gene))),File) :-
	!,
	terms_from_file(File,Terms),
	forall(member(T,Terms), gene_valid(T)).