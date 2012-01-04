
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


