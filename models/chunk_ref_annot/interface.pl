% 
% Adds a "golden standard" reference sequence track to the extra field of
% a chunk
:- ['../../lost.pl'].
:- lost_include_api(io).

add_reference_track([ChunkFile,GoldenStandardFile],_,OutFile) :-
	file_functor(GoldenStandardFile,RefFunctor),
	open(ChunkFile,read,InStream),
	open(OutFile,write,OutStream),
	[GoldenStandardFile],
	add_reference_track_stream(RefFunctor,InStream,OutStream),
	close(InStream),
	close(OutStream).
	
add_reference_track_stream(RefFunctor,InStream,OutStream) :-
	read(InStream,Chunk),
	((Chunk==end_of_file) ->
		true
	;
		Chunk =.. [ Functor, SeqId, Left, Right, Strand, Frame, Extra],
		(lookup_ref_term(RefFunctor,Left,Right,Strand,Frame,Match) ->
			Match =.. [ RefFunctor,_,RefLeft,RefRight,_,_,_],
			PaddingLeft is abs(Left - RefLeft),
			PaddingRight is abs(Right - RefRight),
			GeneLength is 1 + abs(RefLeft-RefRight),
			makelist(PaddingLeft,0,PaddingLeftList),
			makelist(PaddingRight,0,PaddingRightList),
			makelist(GeneLength,1,GenePadding),
			append(PaddingLeftList,GenePadding,Seq1),
			append(Seq1,PaddingRightList, Seq2),
			((Strand='+') ->
				RefAnnot = Seq2
				;
				reverse(Seq2,RefAnnot)
			)
			;
			ChunkLength is 1 + abs(Left - Right),
			makelist(ChunkLength,0,RefAnnot)
		),
		NewExtra = [ref_annot(RefAnnot)|Extra],
		NewChunk =.. [ Functor,SeqId,Left,Right,Strand,Frame,NewExtra],
		writeq(OutStream,NewChunk),
		writeln(OutStream,'.'),
		!,
		add_reference_track_stream(RefFunctor,InStream,OutStream)).

% This disregards overlapping genes in same frame, just pick first match..
lookup_ref_term(RefFunctor,Left,Right,Strand,Frame,FirstMatch) :-
	findall(Match, find_matches(RefFunctor,Left,Right,Strand,Frame,Match),Matches),
	length(Matches,MatchesLen),
	((MatchesLen > 0) ->
		write('found '), 
		write(MatchesLen)
		;
		write('found 0')),
	write(' matches to chunk '),
	write([Left,Right,Strand,Frame]),nl,
	Matches = [FirstMatch|_].

find_matches(RefFunctor,MinLeft,MaxRight,Strand,Frame,Match) :-
	Match =.. [ RefFunctor, _SeqId, Left, Right, Strand, Frame, _ ],
	call(Match),
	Left >= MinLeft,
	Right =< MaxRight.
	
makelist(0,_,[]).
makelist(N,X,[X|Rest]):-
   		N > 0,
        M is N-1,
        makelist(M,X,Rest).
