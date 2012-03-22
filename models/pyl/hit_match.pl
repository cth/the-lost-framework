% Find and filter pylis-orfs which overlap a known gene by at least xxx bases

:- use(genedb).

hit_match(MinOverlap,HitsFile,PylisFile,OutputFile) :-
	terms_from_file(PylisFile,PylisORFs),
	terms_from_file(HitsFile,BlastHits),
	open(OutputFile,write,OutStream),
	report_overlaps(MinOverlap,BlastHits,PylisORFs,OutStream),
	close(OutStream).
	
no_rna_overlap(HitsFile,RNAFile,OutputFile) :-
	terms_from_file(RNAFile,RNAs),
	open(HitsFile,read,HitsIn),
	open(OutputFile,write,HitsOut),
	annotate_hits_with_rnas(HitsIn,RNAs,HitsOut),
	close(HitsIn),
	close(HitsOut).

annotate_hits_with_rnas(HitsIn,RNAs,HitsOut) :-
	read(HitsIn,Hit),
	((Hit == end_of_file) ->
		true
		;
		(find_rna_match(Hit,RNAs,RNA) ->
			gene_add_extra_field(Hit,rna_overlap,RNA,HitUpdated)
			;
			HitUpdated = Hit
		),
		writeq(HitsOut,HitUpdated),
		write(HitsOut,'.\n'),
		!,
		annotate_hits_with_rnas(HitsIn,RNAs,HitsOut)).
	
find_rna_match(Hit,RNAs,RNA) :-
	writeln(find_rna_match),
	gene_extra_field(Hit,hit_left,HitLeft),
	gene_extra_field(Hit,hit_right,HitRight),
	member(RNA,RNAs),
	gene_left(RNA,RNALeft),
	gene_right(RNA,RNARight),
	overlap_length((HitLeft,HitRight),(RNALeft,RNARight),OL),
	OL > 0,
	writeln(overlap_length((HitLeft,HitRight),(RNALeft,RNARight),OL)).	
	
report_overlaps(_,[],_,_).

report_overlaps(MinOverlap,[Hit|RestHits],PylisOrfs,OutStream) :-
	gene_extra_field(Hit,hit_left,HitLeft),
	gene_extra_field(Hit,hit_right,HitRight),
	gene_extra_field(Hit,hit_strand,HitStrand),
	member(ORF,PylisOrfs),
	gene_left(ORF,Left),
	gene_right(ORF,Right),
	gene_strand(ORF,Strand),
	gene_extra_field(ORF,in_frame_stops,[AmberPosition|_]),
	((Strand == '+') ->
		RealLeft = AmberPosition,
		RealRight is AmberPosition + 100
		;
		RealLeft is AmberPosition - 100,
		RealRight = AmberPosition
	),
	HitStrand == Strand,
	overlap_length((RealLeft,RealRight),(HitLeft,HitRight),OverlapLength),
	OverlapLength >= MinOverlap,
	writeln(overlap_length((RealLeft,RealRight),(HitLeft,HitRight),OverlapLength)),	
	gene_add_extra_field(Hit,match_to,ORF,HitWithORF),
/*	writeln(qpos(RealLeft,RealRight)),
	MatchLen is 1+RealRight - RealLeft,
	writeln(matchlen(MatchLen)),
	HLen is 1+HitRight - HitLeft,
	writeln(hpos(HitLeft,HitRight)),
	writeln(hitlen(HLen)),
	writeln(overlap(OverlapLength)),
	*/
	writeq(OutStream,HitWithORF),
	write(OutStream,'.\n'),
	!,
	report_overlaps(MinOverlap,RestHits,PylisOrfs,OutStream).
	
report_overlaps(MinOverlap,[_|RestHits],PylisOrfs,OutStream) :-
	!,
        writeln('no hit'),
	report_overlaps(MinOverlap,RestHits,PylisOrfs,OutStream).
	
	
% Swap if inverse order	
overlap_length((Left1,Right1),(Left2,Right2),L) :-
	Left1 > Left2,
	!,
	overlap_length((Left2,Right2),(Left1,Right1),L).
	
% No overlap
% L1    R1
% |-----|
%          |----| 
%         L2   R2
overlap_length((Left1,Right1),(Left2,Right2),0) :-
 	Right1 < Left2,
	!.
	
% L1         R1
% |----------|
%    |----|   
%    L2   R2
overlap_length((Left1,Right1),(Left2,Right2),OverlapLength) :-
	Right1 =< Right2,
	OverlapLength is 1 + Right1 - Left1.
	
% L1         R1
% |----------|
%     |----------|   
%     L2        R2
overlap_length((Left1,Right1),(Left2,Right2),OverlapLength) :-
	Right1 =< Right2,
	OverlapLength is 1 + Right1 - Left2.
