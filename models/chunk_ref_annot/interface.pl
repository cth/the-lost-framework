% 
% Adds a "golden standard" reference sequence track to the extra field of
% a chunk
:- ['../../lost.pl'].
:- lost_include_api(io).

:- task(add_reference_track([text(prolog(ranges(gene))), text(prolog(ranges(gene)))],[],text(prolog(ranges(gene))))).
:- task(matching_genes([text(prolog(ranges(gene))), text(prolog(ranges(gene)))],[],text(prolog(ranges(gene))))).

%% add_reference_track(+InputFiles,+Options,+OutputFile)
% ==
% InputFiles = [ PutativeFile, ReferenceFile ]
% ==
% adds and extra field to each gene in =|PutativeFile|= if the are a (partically) overlapping gene in =|ReferenceFile|= with same strand+frame.
% The extra field contains a list, which 
% has zeroes in all non-coding positions and ones in the coding positions (of the same strand reading frame).
add_reference_track([ChunkFile,GoldenStandardFile],_,OutFile) :-
	file_functor(GoldenStandardFile,RefFunctor),
	open(ChunkFile,read,InStream),
	open(OutFile,write,OutStream),
	[GoldenStandardFile],
	add_reference_track_stream(RefFunctor,InStream,OutStream),
	close(InStream),
	close(OutStream).

%% matching_genes(+InputFiles,+Options,+OutputFile)
% InputFiles = [ File1, File2 ]
% OutputFile is all the gene entries in File1 which have identical the Left, Right, Frame and Strand to an of entry in File2.
matching_genes([ChunkFile,RefFile],_,OutputFile) :-
	file_functor(RefFile,RefFunctor),
	open(ChunkFile,read,InStream),
	open(OutputFile,write,OutStream),
	[RefFile],
	report_matches_to_chunks_stream(RefFunctor,InStream,OutStream),
	close(InStream),
	close(OutStream).

merge_extra_fields([ChunkFile1,ChunkFile2],_,OutFile) :-
	open(ChunkFile1,read,InStream1),
	open(ChunkFile2,read,InStream2),
	open(OutFile,write,OutStream),
    merge_chunk_extra_fields_stream(InStream1,InStream2,OutStream),
	close(InStream),
	close(OutStream).

merge_chunk_extra_fields_stream(InStream1,InStream2,OutStream) :-
	read(InStream1,Chunk1),
	read(InStream2,Chunk2),
	((Chunk1==end_of_file) ->
		true
	;
		Chunk1 =.. [ Functor, SeqId, Left, Right, Strand, Frame, Extra1],
		Chunk2 =.. [ _Functor, _SeqId, Left, Right, Strand, Frame, Extra2],
                union(Extra1,Extra2,NewExtra),
        	NewChunk =.. [ Functor,SeqId,Left,Right,Strand,Frame,NewExtra],
		writeq(OutStream,NewChunk),
		writeln(OutStream,'.'),
		!,
		merge_chunk_extra_fields_stream(InStream1,InStream2,OutStream)).



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
        RefAnnotFact =.. [RefFunctor,RefAnnot],
		NewExtra = [RefAnnotFact|Extra],
		NewChunk =.. [ Functor,SeqId,Left,Right,Strand,Frame,NewExtra],
		writeq(OutStream,NewChunk),
		writeln(OutStream,'.'),
		!,
		add_reference_track_stream(RefFunctor,InStream,OutStream)).

report_matches_to_chunks_stream(RefFunctor,InStream,OutStream) :-
	read(InStream,Chunk),
	((Chunk==end_of_file) ->
		true
	;
        write('.'),
		Chunk =.. [ _, _, Left, Right, Strand, Frame, _],
        findall(M,find_matches(RefFunctor,Left,Right,Strand,Frame,M),Matches),
        forall(member(Match,Matches),( writeq(OutStream,Match), write(OutStream,'.\n'))),
	    !,
	    report_matches_to_chunks_stream(RefFunctor,InStream,OutStream)).

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
	Match =.. [ RefFunctor, SeqId, Left, Right, Strand, Frame, Extra ],
	call(Match),
	Left >= MinLeft,
	Right =< MaxRight.
	
% create a list if containing N 
makelist(0,_,[]).
makelist(N,X,[X|Rest]):-
   		N > 0,
        M is N-1,
        makelist(M,X,Rest).
