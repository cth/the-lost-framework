% Find and filter pylis-orfs which overlap a known gene by at least xxx bases

filter_by_overlap(MaxOverlap,OrfsFile,KnownGenesFile,OutputFile) :-
	terms_from_file(KnownGenesFile,KnownGenes),
	terms_from_file(OrfsFile,CandidateOrfs),
	open(OutputFile,write,OutStream),
	report_overlaps(MaxOverlap,CandidateOrfs,KnownGenes,OutStream),
	close(OutStream).
	
filter_overlaps(_,[],_KnownGenes,[]).

filter_overlaps(MaxOverlap,[ORF|RestORFs],KnownGenes,NonOverlapping) :-
	(foreach(Gene in KnownGenes, not(overlap(MaxOverlap,ORF,Gene))) ->
		NonOverlapping = [ORF|RestNonOverlapping]
		;
		NonOverlapping = RestNonOverlapping),
	filter_overlaps(MaxOverlap,RestORFs,KnownGenes,RestNonOverlapping).

overlap(MaxOverlap,ORF,G) :-
	gene_left(G,GeneLeft),
	gene_right(G,GeneRight),
	gene_strand(G,GeneStrand),
	gene_frame(G,GeneFrame),
	gene_strand(ORF,OrfStrand),
	gene_frame(ORF,OrfFrame),
	gene_extra_field(ORF,starts,StartCodons),
	reverse(StartCodons,[FirstUpstreamStart,_]),
	[GeneStrand,GeneFrame] \= [OrfStrand,OrfFrame], % Fail if we are talking about same reading frame
	((OrfStrand == '+') ->
		OrfMatchLeft = FirstUpstreamStart,
		gene_right(ORF,OrfMatchRight)
		;
		gene_left(ORF,OrfMatchLeft),
		gene_left(ORF,OrfMatchRight)
	),
	overlap_length((OrfMatchLeft,OrfMatchRight),(GeneLeft,GeneRight),OverlapLength),
	OverlapLength =< MaxOverlap.
	
% Swap if inverse order	
overlap_length((Left1,Right1),(Left2,Right2),L) :-
	Left1 > Left2,
	!,
	overlap_length((Left2,Right2),(Left1,Right1),L).
	
% No overlap
overlap_length((_Left1,Right1),(Left2,_Right2),0) :-
 	Right1 < Left2,
	!.
	
overlap_length((Left1,Right1),(Left2,Right2),OverlapLength) :-
	sort([Left1,Right1,Left2,Right2], [_P1,P2,P3,_P4]),
	OverlapLength is 1 + P3 - P2.