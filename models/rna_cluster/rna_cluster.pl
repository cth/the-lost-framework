% RNA clustering algorithm
:- ['../../lost.pl'].
:- ['stem_constraints'].
:- use(lists).
:- use(genedb).

% Configuration
minimal_stem_length(4).

% Calculate the number of paired bases in each Gene record
count_paired_bases(StructureSequenceFunctor,GeneTerm,UpdatedGeneTerm) :-
	gene_extra_field(GeneTerm,StructureSequenceFunctor,StructureSequence),
	subtract(StructureSequence,['.'],PairedBases),
	length(PairedBases,NumPairedBases),
	gene_add_extra_field(GeneTerm,paired_bases,NumPairedBases,UpdatedGeneTerm).

sort_by_paired_bases(Terms,SortedTermsWithCounts) :-
	map(count_paired_bases(folding,input,output),Terms,TermsWithCounts),
	map(by_counts(input,output), TermsWithCounts, TermsByCount),
	sort(TermsByCount,TermsByCountSorted),
	map(by_counts(output,input), TermsByCountSorted, SortedTermsWithCounts).

by_counts(GeneTerm,(Count,GeneTerm)) :-
	gene_extra_field(GeneTerm,paired_bases,Count).


filter_by_constraints([],[]).
	
filter_by_constraints([G|Xs], [G|Ys]) :-
	apply_sequence_constraints(G),
	!,
	filter_by_constraints(Xs,Ys).

filter_by_constraints([_|Xs], Ys) :-
	filter_by_constraints(Xs,Ys).
		
	
apply_sequence_constraints(GeneTerm) :-
	minimal_stem_length(MinStemLength),
	gene_extra_field(GeneTerm,folding,StructureSequence),
	sequence_with_stem(MinStemLength,StructureSequence,[]).
	
	
create_alignments(InputFile,Alignments) :-
	terms_from_file(InputFile,Terms),
	length(Terms,NumTerms),
	write('read '), write(NumTerms), writeln(' terms from file.'),
	filter_by_constraints(Terms,FilteredTerms),
	length(FilteredTerms,NumFilteredTerms),
	write('After constraint filters, '), write(NumFilteredTerms), writeln(' left. '),
	sort_by_paired_bases(FilteredTerms,SortedTerms),
	writeln('sorted terms and added paired bases counts.'),
	Threshold = 2,
	align_genes(Threshold,SortedTerms,Alignments),
	length(Alignments,NumAlignments),
	
	
%	writeln(Alignments),
	write('created '), write(NumAlignments), write(' alignments.').

align_genes(_T,[],[]).
align_genes(_T,[_],[]).

align_genes(Threshold,[Gene|Rest],Alignments) :-
	align_with_relevant(Gene,Rest,Threshold,AlignmentsFirst),
	align_genes(Threshold,Rest,AlignmentsRest),
	append(AlignmentsFirst,AlignmentsRest,Alignments).
	
align_with_relevant(_Gene,[],_Threshold,[]).
	
align_with_relevant(Gene,[OtherGene|Rest],Threshold,[Alignment|AlignmentsRest]) :-
	gene_extra_field(Gene,paired_bases,GenePairedBases),
	gene_extra_field(OtherGene,paired_bases,OtherGenePairedBases),
	Difference is OtherGenePairedBases - GenePairedBases,
	Difference < Threshold,
	!,
	align(Gene,OtherGene,Alignment),
	align_with_relevant(Gene,Rest,Threshold,AlignmentsRest).
	
align_with_relevant(_Gene,[_OtherGene|_],_Threshold,[]).

% place holder
%align(_A,_B,0).
align(A,B,(IdA,IdB,Cost)) :-
	gene_extra_field(A,folding,FoldingA),
	gene_extra_field(B,folding,FoldingB),
	sequence_id(A,IdA),
	sequence_id(B,IdB),
	stupid_align(FoldingA,FoldingB,Cost).


sequence_id(GeneTerm,(SeqId,Left,Right)) :-
	gene_left(GeneTerm,Left),
	gene_right(GeneTerm,Right),
	gene_sequence_id(GeneTerm,SeqId).
	
% Very stupid alignment algorithm that just counts the number of mismatches
stupid_align([],[],0).
	
stupid_align([Same|As],[Same|Bs],Score) :-
	stupid_align(As,Bs,Score).

stupid_align([A|As],[B|Bs],Score) :-
	A \= B,
	stupid_align(As,Bs,RestScore),
	Score is RestScore + 1.
	
create_distance_file(Alignments) :-
	open('distance_matrix.pl',write,OS),
	write_alignments_to_stream(Alignments,OS),
	close(OS).

write_alignments_to_stream([],_).
write_alignments_to_stream([(A,B,Dist)|Rest],OS) :-
	write(OS,distance(A,B,Dist)),
	write(OS,'.\n'),
	write_alignments_to_stream(Rest,OS).

test :-
	create_alignments('ppfold_fold_33.gen',Alignments),
	create_distance_file(Alignments).
