% RNA clustering algorithm
:- ['../../lost.pl'].

:- use(lists).
:- use(genedb).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Sequence constraints 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

is_valid_alignment(Gene,OtherGene) :-
    max_pairing_mismatch(Threshold),
	gene_extra_field(Gene,paired_bases,GenePairedBases),
	gene_extra_field(OtherGene,paired_bases,OtherGenePairedBases),
	Difference is OtherGenePairedBases - GenePairedBases,
	Difference < Threshold.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Alignment 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

build_alignments(InputFile,OutputFile) :-
	terms_from_file(InputFile,Terms),
	writeln('sorting by paired bases'),
	sort_by_paired_bases(Terms,SortedTerms),
	writeln('sorted terms and added paired bases counts.'),
	write('aligning sequences...'),
    open(OutputFile,write,AlignedStream),
	align_genes(SortedTerms,AlignedStream),!,nl,
    close(AlignedStream),
	writeln('done.').

align_genes(_T,[],_).
align_genes(_T,[_],_).

align_genes([Gene|Rest],OutStream) :-
    write('.'),
%	initialize_table,
	align_with_relevant(Gene,Rest,Alignments),
    write_alignments_to_stream(Alignments,OutStream),
	align_genes(Rest,OutStream).

write_alignments_to_stream([],_Stream).
write_alignments_to_stream([(Dist,(A,B))|As],Stream) :-
        writeq(Stream,[Dist,A,B]),
        write(Stream,'.\n'),
        write_alignments_to_stream(As,Stream).
	
align_with_relevant(_Gene,[],[]).
	
align_with_relevant(Gene,[OtherGene|Rest],[Alignment|AlignmentsRest]) :-
	is_valid_alignment(Gene,OtherGene),
	!,
	align(Gene,OtherGene,Alignment),!,
	align_with_relevant(Gene,Rest,AlignmentsRest).
	
align_with_relevant(Gene,[_OtherGene|Rest],AlignmentsRest) :-
	align_with_relevant(Gene,Rest,AlignmentsRest).

	
align(A,B,(Cost,IdA,IdB)) :-
	sequence_id(A,IdA),
	sequence_id(B,IdB),
	align_method(Method),
	AlignGoal =.. [ Method, A, B, Cost ],
	call(AlignGoal).
	
pseudo_align(A,B,(inf,IdA,IdB)) :-
	sequence_id(A,IdA),
	sequence_id(B,IdB).

sequence_id(GeneTerm,(SeqId,Left,Right)) :-
        gene_extra_field(GeneTerm,in_frame_stops,[PylisStart]),
        (gene_strand(GeneTerm,'+') ->
                Left = PylisStart,
                Right is (100 + PylisStart) - 1
                ;
                Left is (PylisStart - 100) + 1,
                Right = PylisStart

        ),
	gene_sequence_id(GeneTerm,SeqId).

% In the case that the sequence does not have in_frame_stops
% we just use left and right
sequence_id(GeneTerm,(SeqId,Left,Right)) :-
        gene_sequence_id(GeneTerm,SeqId),
        gene_left(GeneTerm,Left),
        gene_right(GeneTerm,Right).

