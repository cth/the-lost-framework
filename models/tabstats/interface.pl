% tab_range_stats
% range stats in tab separated format

:- ['../../lost.pl'].

:- lost_include_api(io).
:- lost_include_api(misc_utils).

% Simple Prolog debugging trick
codon_stats([StatsFile],_Options,TabFile) :-
	writeln(codon_stats),
	terms_from_file(StatsFile,Genes),
	length(Genes,NumGenes),
	write('read '),write(NumGenes),write(' genes'),nl,
	open(TabFile,write,Stream),
	forall(	member(Gene,Genes),
			(
			Gene =.. [ _Functor, _Start, _End, _Frame, _Strand, Extra ],
			member(gene_name(Name),Extra),
			write(Stream,Name),
			write(Stream,'\t'),
			codon_stats(Gene,Stats),
			write_tabsep(Stats,Stream),
			write(Stream,'\n'))
	),
	close(Stream).
		
codon_stats(Gene, Stats) :-
	Gene =.. [ _Functor, _Start, _End, _Frame, _Strand, Extra ],
	member(nucleotide_stats(NucleotideStats),Extra),
	findall(Stat, 
			(
			member(B1,[a,g,c,t]),
			member(B2,[a,g,c,t]),
			member(B3,[a,g,c,t]),
			member(stat([B1,B2,B3],Stat),NucleotideStats)
			), 
			Stats).
	
write_tabsep([One],Stream) :-
	write(Stream,One).
write_tabsep([One|More],Stream) :- 
	write(Stream,One),
	write(Stream,'\t'),
	write_tabsep(More,Stream).