stop_trimmer_rec(InStream,OutStream) :-
	read(InStream,Term),
	((Term==eof) ->
		true
		;
		Term =.. [ Functor, SeqId, Left, Right, Strand, Frame, Extra ],
		member(hseq(Line),Extra),
		trim_line_for_stops(['*'],Line,TrimmedLine),
		length(Line,LineLength),
		length(TrimmedLine,TrimmedLineLength),
		NumSymbolsTrimmed is LineLength - TrimmedLineLength,
		makelist(NumSymbolsTrimmed,0,NewPrefix),
		append(NewPrefix,TrimmedLine,TrimSeq),
		NewExtra = [ trimmed_hseq(TrimSeq) | Extra ], 
		NewTerm =.. [ Functor, SeqId, Left, Right, Strand, Frame, NewExtra ],
		write(OutStream,NewTerm),
		stop_trimmer_rec(InStream,OutStream)
	).


trim_line_for_stops(StopSymbols,Line) :-
	reverse(Line,ReverseMidline),
	trim_until(StopSymbols,ReverseMidline,ReverseMidlineTrimmed),
	reverse(ReverseMidlineTrimmed).

trim_until(StopSymbols,[Sym|_],AccFinal,AccFinal) :- member(Sym,StopSymbols).
trim_until([X|Rest],Acc,Final) :-
	not(member(Stop,StopSymbols)),
	trim_until_stop(Rest,[X|Acc],Final).
		
makelist(0,_,[]).
makelist(N,X,[X|Rest]):-
        N > 0,
        M is N-1,
        makelist(M,X,Rest).

