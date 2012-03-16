:- use(genedb).
:- use(lists).

% inframe_stop_sequences(+OrfsFile,+OutputFile)
% Reads each term from OrfsFile in a Stream
inframe_stop_sequences(OrfsFile,OutputFile,MaxBasesDownstream) :-
	open(OrfsFile,read,InStream),
	open(OutputFile,write,OutStream),
	add_aug_stream_term(0,InStream,OutStream,MaxBasesDownstream),
	close(InStream),
	close(OutStream).

add_aug_stream_term(N,InStream,OutStream,MaxBasesDownstream) :-
	read(InStream,OrfTerm),
	((OrfTerm == end_of_file) ->
		true
		;
		((0 is N mod 1000) -> write(N) ; ((0 is N mod 100) -> write('.') ; true)),
		N1 is N + 1,
		add_aug_term(OrfTerm,OutStream,MaxBasesDownstream),
		add_aug_stream_term(N1,InStream,OutStream,MaxBasesDownstream)
	).

add_aug_term(Orf,OutStream,MaxBasesDownstream) :-
	gene_extra_field(Orf,in_frame_stops,[InFrameStop|_]), % There is never more than one
	gene_extra_field(Orf,sequence,Sequence),
	gene_start_codon(Orf,Start),
	extract_uag_downstream(Sequence,Start,MaxBasesDownstream,InFrameStop,DownstreamSequence),
	gene_add_extra_field(Orf,pylis_sequence,DownstreamSequence,UpdatedOrf),
	writeq(OutStream,UpdatedOrf),
	write(OutStream,'.\n').

extract_uag_downstream(Sequence,Start,MaxBasesDownstream,InFrameStop,UAG_Downstream) :-
	distance(Start,InFrameStop,PrefixSize),
	PrefixSize1 is PrefixSize - 1, % adjust to get also the t of t,a,g
	prefix(PrefixSize1,Sequence,_Prefix,Suffix),
    length(Suffix,SuffixLength),
    ((SuffixLength < MaxBasesDownstream) ->
		UAG_Downstream = Suffix
		;
		prefix(MaxBasesDownstream,Suffix,UAG_Downstream,_Rest)
	).
	
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
