% Given a file containing a of list genes, various statistics will be 
% added to the extra list for each gene

:- ['../../lost.pl'].

:- lost_include_api(sequence).
:- lost_include_api(stats).
:- lost_include_api(misc_utils).

% Simple Prolog debugging trick
:- op(800, fx,'>').
'>'(X) :- writeq(X), write('\n'), call(X).


lost_option(lost_best_annotation,max_nucleotide_order,1, 'Nucleotide stats will be build upto this order.').
lost_option(lost_best_annotation,max_codon_order,1, 'Codon stats will be build upto this order.').
lost_option(lost_best_annotation,max_amino_acid_order,0, 'Amino acid stats will be build upto this order.').
lost_option(lost_best_annotation,genecode,11,'The genetic code to use for translation').

% The main model predicate:
lost_best_annotation([GenesFile,GenomeFile], Options, OutputFile) :-
  terms_from_file(GenesFile,Terms),
  load_sequence(genome,GenomeFile),
  max_gene_length(Terms,0,MaxGeneLength),
  add_gene_statistics(Options,MaxGeneLength,Terms,TermsWithStats),
  terms_to_file(OutputFile,TermsWithStats).

% Length proportional to average length.

max_gene_length([],Max,Max).
max_gene_length([GT|GTRest],CurrentMax,BestMax) :-
        gene_length(GT,Length),
        ((Length > CurrentMax) ->
                NextCurrentMax = Length
                ;
                NextCurrentMax = CurrentMax),
        max_gene_length(GTRest,NextCurrentMax,BestMax).

gene_length(GeneTerm, Length) :-
	GeneTerm =.. [ _, Start, End, _, _, _ ],
        Length is (End - Start) + 1.


add_gene_statistics(_,_,[],[]).

add_gene_statistics(Options,MaxGeneLength,[GT|GTRest], [GTStat|GTStatRest]) :-
        write('add_gene_statistics'),nl,
        !,
	GT =.. [ Functor, Start, End, Frame, Strand, ExtraList ],
	write(GT),nl,
	
	% Extract relevant part of genome
	((Frame == '+') ->
		get_sequence_range(genome,Start,End,Nucleotides)
		;
		get_sequence_range(genome,Start,End,NucleotidesRevComp),
                reverse(NucleotidesRevComp,NucleotidesComp),
		dna_seq_complement(NucleotidesComp,Nucleotides)
	),

	% Calculate nucleotide stats
	get_option(Options,max_nucleotide_order,MaxNucOrder),
	downto(MaxNucOrder,0,NucleotideOrdersList),
	calculate_multiple_stats(nucleotide,NucleotideOrdersList,Nucleotides,NucleotideStats),
        !,

	% Calculate amino acid stats
	get_option(Options,max_amino_acid_order,AAOrder),
	downto(AAOrder,0,AAOrdersList),
        get_option(Options,genecode,GeneCode),
	dna_translate(GeneCode,Nucleotides,AminoAcids),
	calculate_multiple_stats(amino_acid,AAOrdersList,AminoAcids,AminoAcidStats),
        !,

        % Add normalized gene length measure
        gene_length(GT,GeneLength),
        NormGeneLength is GeneLength / MaxGeneLength,
       
	GTStat =.. [ Functor, Start, End, Frame, Strand, 
                     [ amino_acid_stats(AminoAcidStats), nucleotide_stats(NucleotideStats), normalized_gene_length(NormGeneLength) | ExtraList]],
	%write('new: '), write(GTStat),nl,
	add_gene_statistics(Options,MaxGeneLength,GTRest,GTStatRest).
	
calculate_multiple_stats(_,[],_,[]).

calculate_multiple_stats(Type,[Order|OrdersRest],Sequence,StatFacts) :-
	stats(Type,[order(Order)],Sequence,_,Stats),
        !,
	build_stat_facts(Stats,StatFacts1),
        %write(StatFacts1),nl,
        normalize_stat_facts(StatFacts1,StatFactsNorm),
        %write(StatFactsNorm),nl,
	calculate_multiple_stats(Type,OrdersRest,Sequence,StatFactsRest),
	append(StatFactsNorm,StatFactsRest,StatFacts).

downto(To,To,[To]).
downto(From,To,[From|Rest]) :-
	NextFrom is From - 1,
	downto(NextFrom,To,Rest).
