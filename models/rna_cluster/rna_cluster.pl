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


read_and_filter_terms(Stream,FilteredTerms,TermsRead) :-
	read(Stream,Term),
	((Term == end_of_file) ->
		nl
		;
		((0 is TermsRead mod 100) -> write('.') ; true),
		((0 is TermsRead mod 1000) -> write(TermsRead) ; true),
		(apply_sequence_constraints(Term) ->
			FilteredTerms = [Term|FilterRest]
			;
			FilteredTerms = FilterRest
		),
		TermsRead1 is TermsRead + 1,
		!,
		read_and_filter_terms(Stream,FilterRest,TermsRead1)).
		
			


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

create_alignments(InputFile,OutputFile) :-
/*	terms_from_file(InputFile,Terms),
	length(Terms,NumTerms),
	write('read '), write(NumTerms), writeln(' terms from file.'),
	filter_by_constraints(Terms,FilteredTerms),
*/
	open(InputFile,read,IS),
	read_and_filter_terms(IS,FilteredTerms,1),
	close(IS),
	
	length(FilteredTerms,NumFilteredTerms),
	write('After constraint filters, '), write(NumFilteredTerms), writeln(' left. '),
	sort_by_paired_bases(FilteredTerms,SortedTerms),
	writeln('sorted terms and added paired bases counts.'),
	Threshold = 4,
	writeln('aligning sequences...'),
        open(OutputFile,write,AlignedStream),
	align_genes(Threshold,SortedTerms,AlignedStream),
        close(AlignedStream),
	writeln('done.').
/*
	flatten(NestAlignments,Alignments),
	writeln('done.'),
	length(Alignments,NumAlignments),
	write('created '), write(NumAlignments), write(' alignments.'),
        SortedAlignments = Alignments,
        open('test',write,TEST),
        write(TEST,Alignments),
        told,
        writeln(here),nl.
%        writeln(Alignments),
%	sort(Alignments,SortedAlignments).
%	*/

align_genes(_T,[],_).
align_genes(_T,[_],_).

align_genes(Threshold,[Gene|Rest],OutStream) :-
        write('.'),
        initialize_table, 
        % We clear the table area for each new gene 
        % This will keep memory usage down (somewhat)
        % However, we need to make sure that we do not have dangling
        % refs to the table area so we write all alignments to file 
        % before clearing again.
	align_with_relevant(Gene,Rest,Threshold,Alignments),
        write_alignments_to_stream(Alignments,OutStream),
	align_genes(Threshold,Rest,OutStream).

write_alignments_to_stream([],_Stream).
write_alignments_to_stream([(Dist,A,B)|As],Stream) :-
        write(Stream,[Dist,A,B]),
        write(Stream,'.\n'),
        write_alignments_to_stream(As,Stream).
	
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
align(A,B,(Cost,IdA,IdB)) :-
	gene_extra_field(A,folding,FoldingA),
	gene_extra_field(B,folding,FoldingB),
	sequence_id(A,IdA),
	sequence_id(B,IdB),
	edit(FoldingA,FoldingB,Cost).
%	stupid_align(FoldingA,FoldingB,Cost).
%	Cost = 1.
	
sequence_id(GeneTerm,(SeqId,Left,Right)) :-
	gene_left(GeneTerm,Left),
	gene_right(GeneTerm,Right),
	gene_sequence_id(GeneTerm,SeqId).
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Edit distance alignment
% Should be run with b-prolog 7.7 or better
% (otherwise will be inefficient).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- table edit/3.

edit([],[],0).
edit([],[_Y|Ys],Dist) :- edit([],Ys,Dist1), Dist is 1 + Dist1.
edit([_X|Xs],[],Dist) :- edit(Xs,[],Dist1), Dist is 1 + Dist1.

edit([X|Xs],[Y|Ys],Dist) :-
	edit([X|Xs],Ys,InsDist),
	edit(Xs,[Y|Ys],DelDist),
	edit(Xs,Ys,TailDist),
	(X==Y -> 
		% Match
		Dist = TailDist
		; 
		InsDist1 is InsDist + 0.25,
		DelDist1 is DelDist + 0.25,
		TailDist1 is TailDist + 1,
		% minimal of insertion, deletion or substitution 
		sort([InsDist1,DelDist1,TailDist1],[Dist|_])).
	
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

write_distances_to_stream([],_).
write_distances_to_stream([(Dist,A,B)|Rest],OS) :-
	write(OS,distance(A,B,Dist)),
	write(OS,'.\n'),
	write_distances_to_stream(Rest,OS).

test :-
%	create_alignments('ppfold_fold_33.gen',Alignments),%
	system('/opt/BProlog/bp -g "cl(rna_cluster), create_alignments(\'ppfold_fold_33.gen\',\'alignments.pl\'), halt."'),
        terms_from_file('alignments.pl',Alignments),
        length(Alignments,NumAligns),
        write(NumAligns), write(' alignments..'),nl,
        writeln('sorting alignments (may take some time)'),
        sort(Alignments,SortedAlignments),
        writeln(done),
        terms_to_file('distance_matrix.pl',SortedAlignments).
	%create_distance_file(Sortedlignments).
