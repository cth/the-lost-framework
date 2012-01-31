:- use(genedb).


a_gene(Gene) :-
	Gene =.. [ gene, 'n/a', 231, 2341, '+', 2, [sequence([a,g,t,c,t,g,a,g,a])]].
	
not_a_gene1(Gene) :-
	Gene =.. [ gene, 'n/a', 231, 2341, '+', 5, [sequence([a,g,t,c,t,g,a,g,a])]].

not_a_gene2(Gene) :-
	Gene =.. [ gene, 'n/a', 231, 2341, '?',2, [sequence([a,g,t,c,t,g,a,g,a])]].	
	

testcase(gene_valid) :-
	a_gene(G),
	not_a_gene1(G1),
	not_a_gene2(G2),
	gene_valid(G),
	not(gene_valid(G1)),
	not(gene_valid(G2)).
	
testcase(gene_sequence_id) :-
	a_gene(G),
	gene_sequence_id(G,'n/a').
	
testcase(gene_left) :-
	a_gene(G),
	gene_left(G,231).
	
testcase(gene_right) :-
	a_gene(G),
	gene_right(G,2341).

testcase(gene_strand) :-
	a_gene(G),
	gene_strand(G,'+').
	
testcase(gene_frame) :-
	a_gene(G),
	gene_frame(G,2).
	
testcase(gene_extra_field) :-
	a_gene(G),
	gene_extra_field(G,sequence,[a,g,t,c,t,g,a,g,a]).
	
testcase(gene_add_extra_field) :-
	a_gene(G),
	gene_add_extra_field(G,test,somevalue,G2),
	gene_extra_field(G2,test,somevalue).	
