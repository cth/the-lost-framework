:- ['../../lost.pl'].

lost_input_formats(trim_stops, [text(prolog(blast_matches))]).
lost_output_format(trim_stops,_, [text(prolog(blast_matches))]).

%%
% Trim stops in matches
% 
% Basically, finds the maximal suffix of all HSPs that do
% contain stop codons 
trim_stops([BlastHspFile],_Options,TrimmedHspFile) :-
	open(BlastHspFile,read,InStream),
	open(TrimmedHspFile,write,OutStream),
	stop_trimmer_rec(InStream,OutStream),
	close(InStream),
	close(OutStream).

%%
% add an identity signal to each blast HSP
identity([BlastHspFile],_,OutFile) :-
	open(BlastHspFile,read,InStream),
	open(OutFile,write,OutStream),
	identity_markup_stream(InStream,OutStream),
	close(InStream),
	close(OutStream).

identity_markup_stream(InStream,OutStream) :-
	read(InStream,Term),
	((Term==end_of_file) ->
		write('done'),nl,
		true
		;
		Term =.. [ Functor, SeqId, Left, Right, Strand, Frame, Extra ],
		member(hseq(HitSeq),Extra),
		(member(real_match(yes),Extra) ->
			member(qseq(QuerySeq),Extra),
			member(query_from(QueryFrom),Extra),
			member(query_to(QueryTo),Extra),
			QueryLength is (1+abs(Left - Right)) // 3,
			AlignmentLength is 1+abs(QueryTo - QueryFrom),
			PaddingStart is abs(QueryFrom-1),
			PaddingEnd is abs(QueryLength - (AlignmentLength + PaddingStart)),
			makelist(PaddingStart,0,PaddingStartList),
			makelist(PaddingEnd,0,PaddingEndList),
			write(PaddingEndList),nl,
			write(mark),
			write(	identity_markup_line(QuerySeq,HitSeq,AlignIdentitySeq)),nl,
			identity_markup_line(QuerySeq,HitSeq,AlignIdentitySeq),
			append(PaddingStartList,AlignIdentitySeq,StartPadded),
			append(StartPadded,PaddingEndList,IdentitySeq)
			;
			IdentitySeq=HitSeq
		),
		NewExtra = [ identity_seq(IdentitySeq) | Extra ],
		NewTerm =.. [ Functor, SeqId, Left, Right, Strand, Frame, NewExtra ],
		write(IdentitySeq),nl,
		length(IdentitySeq,SeqLen),
		write('Length is '), write(SeqLen),nl,
		writeq(OutStream,NewTerm),
		write(OutStream,'.\n'),
		identity_markup_stream(InStream,OutStream)
	).
	
identity_markup_line([],_,[]).
identity_markup_line([C|Qs],[C|Hs],[1|Is]) :-
	!,
	identity_markup_line(Qs,Hs,Is).

% WARNING: This is dubious! Fix blast report instead! (when you get the time..)
%identity_markup_line([x|Qs],[_|Hs],[1|Is]) :-
%	!,
%	identity_markup_line(Qs,Hs,Is).	
	
identity_markup_line([Q|Qs],[H|Hs],[0|Is]) :-
	H \= Q,
	identity_markup_line(Qs,Hs,Is).


%%
% Remove duplicate hits
% Assumption: First one has best score 

remove_dups([BlastHitFile],[],OutFile) :-
	open(BlastHitFile,read,InStream),
	open(OutFile,write,OutStream),
	remove_dups_stream(nil,InStream,OutStream),
	close(InStream),
	close(OutStream).

remove_dups_stream(PrevSeqId,InStream,OutStream) :-
	read(InStream,Term),
	((Term==end_of_file) ->
		write('done'),nl,
		true
		;
		Term =.. [ _, SeqId, _, _, _, _, _ ],
		((SeqId==PrevSeqId) ->
			true % = skip this one
			;
			writeq(OutStream,Term),
			write(OutStream,'.\n')),
		!,
		remove_dups_stream(SeqId,InStream,OutStream)).
%%
% merge_identity 
%
merge_multiple(BlastFiles,_,OutFile) :-
	findall(InStream, (member(File,BlastFiles),open(File,read,InStream)), InStreams),
	open(OutFile,write,OutStream),
	merge_multiple_streams(merge_multiple_lines,InStreams, OutStream),
	close(OutStream),
	forall(member(S,InStreams), close(S)).

sum_multiple(BlastFiles,_,OutFile) :-	
	findall(InStream, (member(File,BlastFiles),open(File,read,InStream)), InStreams),
	open(OutFile,write,OutStream),
	merge_multiple_streams(sum_multiple_lines, InStreams, OutStream),
	close(OutStream),
	forall(member(S,InStreams), close(S)).

merge_multiple_streams(MergeFunctor,InStreams,OutStream) :-
	findall(Term,(member(IS,InStreams),read(IS,Term)),Terms),
	(member(end_of_file,Terms) ->
		write('done'),nl,
		true
		;
%		forall(member(T,Terms),(write(T),nl)),
		findall(IdSeq,(member(T,Terms),identity_seq_from_term(T,IdSeq)),IdSeqs),
		write(IdSeqs),nl,
		MergeGoal =.. [ MergeFunctor, IdSeqs, MergedIdSeqs ],
		call(MergeGoal),
		NewExtra = [ identity_seq(MergedIdSeqs) ], % Note, that this throws original extra away
		write(here3),
		Terms = [T|_],
		T =.. [ Functor, _, Left, Right, Strand, Frame, _ ],
		write([ Functor, merged_sequences, Left, Right, Strand, Frame, NewExtra ]),nl,
		NewTerm =.. [ Functor, merged_sequences, Left, Right, Strand, Frame, NewExtra ],
		write(MergedIdSeqs),nl,
		writeq(OutStream,NewTerm),
		write(OutStream,'.\n'),
		merge_multiple_streams(MergeFunctor,InStreams,OutStream)
	).

identity_seq_from_term(Term,IdSeq) :-
	Term=..[_,_,_,_,_,_,Extra],
	member(identity_seq(IdSeq),Extra).

% Basically, Multi-zip:
merge_multiple_lines(Lines,[]) :-
	forall(member(L,Lines), L==[]). % All lines are empty

merge_multiple_lines(Lines,[Elems|ElemsRest]) :-
	findall(Elem1,(member(Line,Lines),Line=[Elem1|_]), Elems),
	findall(LineRest,(member(Line,Lines),Line=[_|LineRest]), LinesRest),
	merge_multiple_lines(LinesRest,ElemsRest).
	



sum_multiple_lines(Lines,[]) :-
	forall(member(L,Lines), L==[]). % All lines are empty

sum_multiple_lines(Lines,[Sum|ElemsRest]) :-
	findall(Elem1,(member(Line,Lines),Line=[Elem1|_]), Elems),
	sumlist(Elems,Sum),
	findall(LineRest,(member(Line,Lines),Line=[_|LineRest]), LinesRest),
	sum_multiple_lines(LinesRest,ElemsRest).

	

merge_identity([BlastHspFile1,BlastHspFile2],_,OutFile) :-
	!,
	open(BlastHspFile1,read,InStream1),
	open(BlastHspFile2,read,InStream2),
	open(OutFile,write,OutStream),
	sum_merge_identity_streams(InStream1,InStream2,OutStream),
	close(InStream1),
	close(InStream2),
	close(OutStream).


sum_merge_identity_streams(InStream1,InStream2,OutStream) :-
	read(InStream1,Term1),
	read(InStream2,Term2),
	(((Term1==end_of_file),(Term2==end_of_file)) ->
		write('done'),nl,
		true
		;
		Term1 =.. [ Functor, SeqId, Left, Right, Strand, Frame, Extra1 ],
		Term2 =.. [ Functor, SeqId, Left, Right, Strand, Frame, Extra2 ],
		member(identity_seq(IdSeq1),Extra1),
		member(identity_seq(IdSeq2),Extra2),
		sum_identity_lines(IdSeq1,IdSeq2,IdSeqSums),
		NewExtra = [ identity_seq(IdSeqSums) ], % This throws original extra away
		NewTerm =.. [ Functor, SeqId, Left, Right, Strand, Frame, NewExtra ],
		write(IdSeqSums),nl,
		writeq(OutStream,NewTerm),
		write(OutStream,'.\n'),
		sum_merge_identity_streams(InStream1,InStream2,OutStream)
	).

sum_identity_lines([],[],[]).
sum_identity_lines([Q|Qs],[H|Hs],[Sum|SumsRest]) :-
	Sum is Q + H, 	
	sum_identity_lines(Qs,Hs,SumsRest).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Stop trimming - not used anymore
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
stop_trimmer_rec(InStream,OutStream) :-
	read(InStream,Term),
	((Term==end_of_file) ->
		write('done'),nl,
		true
		;
		Term =.. [ Functor, SeqId, Left, Right, Strand, Frame, Extra ],
		member(hseq(Line),Extra),
		%write(trim_line(['*'],Line,TrimmedLine)),nl,
		trim_line(['*'],Line,TrimmedLine),
		length(Line,LineLength),
		length(TrimmedLine,TrimmedLineLength),
		NumSymbolsTrimmed is LineLength - TrimmedLineLength,
		makelist(NumSymbolsTrimmed,0,NewPrefix),
		append(NewPrefix,TrimmedLine,TrimSeq),
		NewExtra = [ trimmed_hseq(TrimSeq) | Extra ], 
		NewTerm =.. [ Functor, SeqId, Left, Right, Strand, Frame, NewExtra ],
		%write(NewTerm),nl,
		writeq(OutStream,NewTerm),
		write(OutStream,'.\n'),
		stop_trimmer_rec(InStream,OutStream)
	).

trim_line(StopSymbols,Line,TrimmedLine) :-
	reverse(Line,[LastSymbol|ReverseLine]),
	trim_until(StopSymbols,ReverseLine,[],TrimmedLine1),
	append(TrimmedLine1,[LastSymbol],TrimmedLine).

trim_until(_,[],AccFinal,AccFinal).

trim_until(StopSymbols,[Sym|_],AccFinal,AccFinal) :- 
	member(Sym,StopSymbols),
	!.
trim_until(StopSymbols,[Sym|Rest],Acc,Final) :-
	trim_until(StopSymbols, Rest,[Sym|Acc],Final).

makelist(0,_,[]).
makelist(N,X,[X|Rest]):-
        N > 0,
        M is N-1,
        makelist(M,X,Rest).

test :-
  L = [h,g,'*',s,l,c,p,g,f,q,c,q,y,e,r,r,v,'*',c,a,r,g,g,g,d,t,c,'*',w,c,i,a,r,r,c,s,h,g,'*'],
  trim_line(['*'],L,LL),
  write(LL).
	
	

