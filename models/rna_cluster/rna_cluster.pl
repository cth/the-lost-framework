% RNA clustering algorithm
:- ['../../lost.pl'].
:- ['stem_constraints'].
:- use(lists).
:- use(genedb).

% Configuration : Assert these...
% minimal_stem_length(4).
% pairing_count_mismatch_threshold(4).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% control predicates and test
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
go :-
        go('ppfold200.pl','tmp_out.newick').
go_all :-
        go('ppfold_fold_33.gen','all.newick').

go(InputFile,OutputFile) :-
		term2atom(assert(minimal_stem_length(4)),StemConstraint),
		term2atom(assert(pairing_count_mismatch_threshold(2)),PairingConstraint),
		assert(minimal_stem_length(4)),
		assert(pairing_count_mismatch_threshold(4)),
        atom_concat_list(['/opt/BProlog/bp -g "cl(rna_cluster), ',
 						StemConstraint, ',', PairingConstraint, ',',
						'create_alignments(\'',
                        InputFile,
                        '\',\'alignments.pl\'), halt."'],
                        BPrologInvoke),
        system(BPrologInvoke),
        terms_from_file('alignments.pl',Alignments),
        length(Alignments,NumAligns),
        write(NumAligns), write(' alignments..'),nl,
        writeln('Sorting alignments (may take some time)'),
        sort(Alignments,SortedAlignments),
        writeln(done),
        terms_to_file('distance_matrix.pl',SortedAlignments),
		slink_cluster_by_distances('distance_matrix.pl',OutputFile).

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


read_and_filter_terms(Stream,FilteredTerms,TermsRead) :-
	read(Stream,Term),
	((Term == end_of_file) ->
		nl
		;
		((0 is TermsRead mod 100) -> write('.') ; true),
		((0 is TermsRead mod 1000) -> write(TermsRead) ; true),
		(apply_sequence_constraints(Term) ->
			write('keeping one..'),nl,
			FilteredTerms = [Term|FilterRest]
			;
			write('skipping one..'),nl,
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Alignment 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_alignments(InputFile,OutputFile) :-
   write('Reading and filtering sequences by constraints..'),
	open(InputFile,read,IS),
	read_and_filter_terms(IS,FilteredTerms,1),
	close(IS),
    writeln(done),
	length(FilteredTerms,NumFilteredTerms),
	write('After constraint filters, '), write(NumFilteredTerms), writeln(' left. '),
	sort_by_paired_bases(FilteredTerms,SortedTerms),
	writeln('sorted terms and added paired bases counts.'),
    pairing_count_mismatch_threshold(Threshold),
	write('aligning sequences...'),
    open(OutputFile,write,AlignedStream),
	align_genes(Threshold,SortedTerms,AlignedStream),
    close(AlignedStream),
	writeln('done.').

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
write_alignments_to_stream([(Dist,(A,B))|As],Stream) :-
        writeq(Stream,[Dist,A,B]),
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
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
% A DCG to convert trees to newick format
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
%

write_newick_tree(Stream,T) :-
	writeln(T),nl,
	newick(T,NewickT,[]),
	atom_codes(NewickA,NewickT),
	write(Stream,NewickA),
	write(Stream,';\n').

% A Leaf node
newick([Dist,NodeId,[]]) --> nodeid(NodeId), ":", str(Dist).

% A Junction 
newick([DistParent,_NodeId,[LeftSubTree,RightSubTree]]) --> 
	"(",
	newick(LeftSubTree),
	",",
	newick(RightSubTree),
	")",
	":",
	str(DistParent).
	
nodeid(Num) --> str(Num).
	
nodeid((Organism,LeftPos,RightPos)) -->
	{
		atom_codes(Organism,OrganismCodes)
	},
	OrganismCodes,
	"_",
	str(LeftPos),
	"-",
	str(RightPos).
	

str(Int) -->
	{
		number(Int),
		number_chars(Int,Chars),
		to_atom_codes(Chars,Codes)
	},
	Codes.

to_atom_codes([],[]).
to_atom_codes([C|Cs], [D|Ds]) :- atom_codes(C,[D]), to_atom_codes(Cs,Ds).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Clustering algorithm control
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

slink_cluster_by_distances(DistancesFile,NewickTreesFile) :-
	compile_chr('slink.pl'),
    init_slink,
	write('Reading distances: '),
	open(DistancesFile,read,Stream),
	read_from_distance_matrix(Stream,0),
	close(Stream), 
	writeln('done.'),
	writeln('Building trees...'),
	build_tree,
	writeln('done.'),
	write('writing trees to newick file: '),
    writeln(NewickTreesFile),
    open(NewickTreesFile,write,OutStream),
    extract_trees(OutStream),
    close(OutStream).
	
read_from_distance_matrix(Stream,Count) :-
	read(Stream,Term),
	((Term == end_of_file) ->
                write('reached end of file'),nl,
		true
		;
                Term = [Distance,A,B],
		((0 is Count mod 100) -> write('.') ; true),
		((0 is Count mod 1000) -> write(Count) ; true),
		Count1 is Count + 1,
		distance(A,B,Distance),
		read_from_distance_matrix(Stream,Count1)
	).
	
