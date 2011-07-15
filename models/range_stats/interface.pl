% Given a file containing a of list genes, various statistics will be 
% added to the extra list for each gene

:- ['../../lost.pl'].

:- lost_include_api(sequence).
:- lost_include_api(stats).
:- lost_include_api(misc_utils).

% Simple Prolog debugging trick
:- op(800, fx,'>').
'>'(X) :- writeq(X), write('\n'), call(X).


lost_option(annotate,max_nucleotide_order,1, 'Nucleotide stats will be build upto this order.').
lost_option(annotate,max_amino_acid_order,0, 'Amino acid stats will be build upto this order.').

lost_option(annotate,amino_acid_stats,yes,'Whether to include amino acid statistics.').
lost_option(annotate,nucleotide_stats,yes,'Whether to include nucleotide statistics.').
lost_option(annotate,length_stats,yes,'Whether to include length statistics.').

lost_option(annotate,genecode,11,'The genetic code to use for translation').

% The main model predicate:
annotate([GenesFile,GenomeFile], Options, OutputFile) :-
  terms_from_file(GenesFile,Terms),
  write('loaded genes...'),nl,
  load_sequence(genome,GenomeFile),
   writeln('loaded sequence..'),nl,
  max_gene_length(Terms,0,MaxGeneLength),
	writeln('calculate max length'),nl,
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
	GeneTerm =.. [ _, _, Start, End, _, _, _ ],
        Length is (End - Start) + 1.


add_gene_statistics(_,_,[],[]).

add_gene_statistics(Options,MaxGeneLength,[GT|GTRest], [GTStat|GTStatRest]) :-
        write('add_gene_statistics'),nl,
        !,
	GT =.. [ Functor, _, Start, End, Frame, Strand, ExtraList ],
	write(GT),nl,

	StatList1 = [], % List to collect computed statistics
	
	% Extract relevant part of genome
	((Frame == '+') ->
		get_sequence_range(genome,Start,End,Nucleotides)
		;
		get_sequence_range(genome,Start,End,NucleotidesRevComp),
                reverse(NucleotidesRevComp,NucleotidesComp),
		dna_seq_complement(NucleotidesComp,Nucleotides)
	),

	% Calculate nucleotide stats
	get_option(Options,nucleotide_stats,IncludeNucleotideStats),
	((IncludeNucleotideStats == yes) ->
	 get_option(Options,max_nucleotide_order,MaxNucOrder),
	 downto(MaxNucOrder,0,NucleotideOrdersList),
	 calculate_multiple_stats(nucleotide,NucleotideOrdersList,Nucleotides,NucleotideStats),
	 StatList2 = [nucleotide_stats(NucleotideStats)|StatList1]
	 ;
	 StatList2 = StatList1
	),!,

	% Calculate amino acid stats
	get_option(Options,amino_acid_stats,IncludeAminoAcidStats),
	((IncludeAminoAcidStats == yes) -> 
	 get_option(Options,max_amino_acid_order,AAOrder),
	 downto(AAOrder,0,AAOrdersList),
	 get_option(Options,genecode,GeneCode),
	 dna_translate(GeneCode,Nucleotides,AminoAcids),
	 calculate_multiple_stats(amino_acid,AAOrdersList,AminoAcids,AminoAcidStats),
	 StatList3 = [amino_acid_stats(AminoAcidStats)|StatList2]
	;
	 StatList3 = StatList2
	),!,

        % Add normalized gene length measure
	get_option(Options,length_stats,IncludeLengthStats),
	((IncludeLengthStats == yes) ->
	 gene_length(GT,GeneLength),
	 NormGeneLength is GeneLength / MaxGeneLength,
	 StatList4 = [normalized_gene_length(NormGeneLength)|StatList3]
	 ;
	 StatList4 = StatList3
	),!,

	append(StatList4,ExtraList,NewExtraList),
	
	GTStat =.. [ Functor, Start, End, Frame, Strand, NewExtraList ],
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
