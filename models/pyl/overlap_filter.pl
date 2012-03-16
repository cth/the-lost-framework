% Find and filter pylis-orfs which overlap a known gene by at least xxx bases

:- use(genedb).

filter_by_overlap(MaxOverlap,OrfsFile,KnownGenesFile,OutputFile) :-
	terms_from_file(KnownGenesFile,KnownGenes),
	terms_from_file(OrfsFile,CandidateOrfs),
	open(OutputFile,write,OutStream),
	report_overlaps(MaxOverlap,CandidateOrfs,KnownGenes,OutStream),
	close(OutStream).
	
report_overlaps(_,[],_KnownGenes,_OutStream).

report_overlaps(MaxOverlap,[ORF|RestORFs],KnownGenes,OutStream) :-
	(foreach(Gene in KnownGenes, not(overlap(MaxOverlap,ORF,Gene))) ->
		writeq(OutStream,ORF),
		write(OutStream,'.\n')
		;
		true),
	!,
	report_overlaps(MaxOverlap,RestORFs,KnownGenes,OutStream).

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
