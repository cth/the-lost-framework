% Find and filter pylis-orfs which overlap a known gene by at least xxx bases

:- use(genedb).

hit_query_match(HitsFile,PylisFile, OutputFile) :-
	terms_from_file(PylisFile,PylisORFs),
	terms_from_file(HitsFile,BlastHits),
	open(OutputFile,write,OutStream),
        writeln(here),
	report_query_overlaps(BlastHits,PylisORFs,OutStream),
	close(OutStream).

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
			HitUpdated = Hit,
                        gene_extra_field(Hit,hit_left,L),
                        gene_extra_field(Hit,hit_right,R),
                        write('no match to '), writeln((L,R))
		),
		writeq(HitsOut,HitUpdated),
		write(HitsOut,'.\n'),
		!,
		annotate_hits_with_rnas(HitsIn,RNAs,HitsOut)).
	
find_rna_match(Hit,RNAs,RNA) :-
	gene_extra_field(Hit,hit_left,HitLeft),
	gene_extra_field(Hit,hit_right,HitRight),
	member(RNA,RNAs),
	gene_left(RNA,RNALeft),
	gene_right(RNA,RNARight),
	overlap_length((HitLeft,HitRight),(RNALeft,RNARight),OL),
        ((OL < 0) -> throw('overlap length less than zero') ; true),
	OL > 0,
	writeln(overlap_length((HitLeft,HitRight),(RNALeft,RNARight),OL)).

report_query_overlaps([],_,_).

report_query_overlaps([Hit|RestHits],PylisOrfs,OutStream) :-
        write('.'),
        gene_left(Hit,Left),
        gene_right(Hit,Right),
        member(ORF,PylisOrfs),
        gene_left(ORF,Left),
        gene_right(ORF,Right),
        gene_add_extra_field(Hit,query_orf,ORF,UpdatedHit),
        writeq(OutStream,UpdatedHit),
        write(OutStream,'.\n'),
        report_query_overlaps(RestHits,PylisOrfs,OutStream).

report_query_overlaps([Hit|_],_,_) :-
        throw(unmatched_hit(Hit)).
	
report_overlaps(_,[],_,_).

report_overlaps(MinOverlap,[Hit|RestHits],PylisOrfs,OutStream) :-
        %writeln(Hit),
	gene_extra_field(Hit,hit_left,HitLeft),
	gene_extra_field(Hit,hit_right,HitRight),
	gene_extra_field(Hit,hit_strand,HitStrand),
	member(ORF,PylisOrfs),
	gene_left(ORF,Left),
	gene_right(ORF,Right),
	gene_strand(ORF,Strand),
        % We require hits to be on same strand!
        % Note that reading frame can be a little of if the hit does not
        % start at t,a,g,...
	HitStrand == Strand, 
	gene_extra_field(ORF,in_frame_stops,[AmberPosition|_]),
	((Strand == '+') ->
		RealLeft = AmberPosition,
		RealRight is AmberPosition + 100
		;
		RealLeft is AmberPosition - 100,
		RealRight = AmberPosition
	),
	once(overlap_length((RealLeft,RealRight),(HitLeft,HitRight),OverlapLength)),
	OverlapLength >= MinOverlap,
      	writeln(overlap_length((RealLeft,RealRight),(HitLeft,HitRight),OverlapLength)),	
       	gene_add_extra_field(Hit,match_to,ORF,HitWithORF),
        writeq(OutStream,HitWithORF),
        write(OutStream,'.\n'),
	!,
	report_overlaps(MinOverlap,RestHits,PylisOrfs,OutStream).

report_overlaps(MinOverlap,[_|RestHits],PylisOrfs,OutStream) :-
        !,
        writeln('no hit'),
        report_overlaps(MinOverlap,RestHits,PylisOrfs,OutStream).

test1 :-
        writeln(here),
        overlap_length((2551020,2551118),(2550876,2553866),L),
        writeln(L).
	
test2 :-
        overlap_length((461370,461444),(4997251,4997349),L),
        writeln(L).

test3 :-
        overlap_length((1424660,1424758),(1424698,1424798),L),
        writeln(L).
	
	
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
	Right1 >= Right2,
	OverlapLength is 1 + Right2 - Left2.
	
% L1         R1
% |----------|
%     |----------|   
%     L2        R2
overlap_length((Left1,Right1),(Left2,Right2),OverlapLength) :-
	Right1 =< Right2,
	OverlapLength is 1 + Right1 - Left2.


overlap_length((Left1,Right1),(Left2,Right2),0) :-
        throw(uncaught_case( (Left1,Right1),(Left2,Right2))).
