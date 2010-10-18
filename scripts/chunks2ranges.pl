chunks2ranges(ChunkPredFile,CdrPredFile):-
	open(ChunkPredFile,read,ChunkPredsStream,[alias(chunksin)]),
	open(CdrPredFile,write,CdrPredsStream,[alias(cdrsin)]),
	chunks2ranges_rec(ChunkPredsStream,CdrPredsStream),!,
	close(CdrPredsStream),
	close(ChunkPredsStream).
	
chunks2ranges_rec(Chunks_in,Cdrs_out):-
	read(Chunks_in,Term),
	(	
		Term \= end_of_file ->
			analyse(Term,Cdrs_out),!,
			chunks2ranges_rec(Chunks_in,Cdrs_out)
	;
		true
	).
		
analyse(Term,Cdrs_out):-	
	Term =.. [_,SeqHandle,Left,_Right,Dir,Frame,Annot],
	find_cdrs(Left,Annot, Cdrs),!,
	report_cdrs(SeqHandle,Dir,Frame,Cdrs,Cdrs_out).

find_cdrs(_,[],[]):-!.	
find_cdrs(Left,Chunk, Cdrs):-	
	(
	member(1,Chunk)->																	/* checks for all zeros, and filters out if so*/
		find_Cdr_start(Chunk,Ncdr_length, Rest_Chunk_1),!,
		Cdr_start is Left + Ncdr_length,
		find_Cdr_stop(Rest_Chunk_1, Cdr_length, Rest_chunk_2),!,
		Cdr_stop is Cdr_start + Cdr_length - 1,
		Next_Left is Cdr_stop + 1,
		Cdrs = [(Cdr_start,Cdr_stop)|Rest_Cdrs],
		find_cdrs(Next_Left,Rest_chunk_2,Rest_Cdrs),!
	;
		Cdrs = []
	).
		
find_Cdr_start(Chunk, Ncdr_length, Rest_chunk):-
	(
	append(Ncdr,[0,1|Rest],Chunk)->
		length(Ncdr,L),
		Ncdr_length is L+1,
		Rest_chunk = [1|Rest]
	;
		Ncdr_length is 0,
		Rest_chunk = Chunk		
	).
	
find_Cdr_stop(Chunk, Cdr_length, Rest_chunk):-
	(
	append(Cdr,[1,0|Rest],Chunk)->
		length(Cdr,L),
		Cdr_length is L+1,
		Rest_chunk = [0|Rest]
	;
		length(Chunk,Cdr_length),
		Rest_chunk = []		
	).
	
report_cdrs(_SeqHandle,_Dir,_Frame,[],_Cdrs_out):- !.
report_cdrs(SeqHandle,Dir,Frame,[(Left,Right)|Rest],Cdrs_out):-
	Term =..['consorf_cdr',SeqHandle,Left,Right,Dir,Frame,[]],
	writeq(Cdrs_out,Term), writeln(Cdrs_out,'.'),
	report_cdrs(SeqHandle,Dir,Frame,Rest,Cdrs_out).
	
	

	
	