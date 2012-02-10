:- use(genedb).

pylis_size(100).

%% extract_candidate_pyles(+OrfsFile,+OutputFile)
% Reads each term from OrfsFile in a Stream
extract_candidate_pylis(OrfsFile,OutputFile) :-
	open(OrfsFile,read,InStream),
	open(OutputFile,write,OutStream),
	process_stream(0,InStream,OutStream),
	close(InStream),
	close(OutStream).	
	
	
process_stream(N,InStream,OutStream) :-
	read(InStream,OrfTerm),
	((OrfTerm == end_of_file) ->
		true
		;
		((0 is N mod 1000) -> write(N) ; ((0 is N mod 100) -> write('.') ; true)),
		N1 is N + 1,
		process_orf_term(OrfTerm,OutStream),
		process_stream(N1,InStream,OutStream)).
	
% Skip ORFs with more than one in frame amber codon
process_orf_term(Orf,_) :-
	gene_extra_field(Orf,in_frame_stops,Stops),
    length(Stops,NumStops),
   	NumStops > 1,
	!.
	
process_orf_term(Orf,_OutStream) :-
	gene_extra_field(Orf,in_frame_stops,[InFrameStop]),
	gene_stop_codon(Orf,Stop),
	distance(InFrameStop,Stop,Distance),
	pylis_size(PylisSize),
	Distance < PylisSize,
	!.
	
process_orf_term(Orf,OutStream) :-
	gene_extra_field(Orf,in_frame_stops,[InFrameStop]),
	gene_extra_field(Orf,sequence,Sequence),
	gene_start_codon(Orf,Start),
	distance(Start,InFrameStop,PrefixSize),
	PrefixSize1 is PrefixSize - 1, % adjust to get also the t of t,a,g
	prefix(PrefixSize1,Sequence,_Prefix,Suffix),
	pylis_size(PylisSize),
	prefix(PylisSize,Suffix,PylisSeq,_Rest),
	gene_add_extra_field(Orf,pylis,PylisSeq,UpdatedOrf),
	writeq(OutStream,UpdatedOrf),
	write(OutStream,'.\n').
	
prefix(0,S,[],S).

prefix(N,[S|Ss],[S|Ps],Suffix) :-
	N1 is N - 1,
	prefix(N1,Ss,Ps,Suffix).
		
distance(Pos1,Pos2,Dist) :-
	Pos1 < Pos2,
	distance(Pos2,Pos1,Dist).
	
distance(Pos1,Pos2,Dist) :-
	Pos1 >= Pos2,
	Dist is (Pos1 - Pos2) + 1.