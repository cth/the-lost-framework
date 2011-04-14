:- ['../../lost.pl'].
:- [stop_trimmer].

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
			identity_markup_line(QuerySeq,HitSeq,IdentitySeq)
			;
			IdentitySeq=HitSeq
		),
		NewExtra = [ identity_seq(IdentitySeq) | Extra ],
		NewTerm =.. [ Functor, SeqId, Left, Right, Strand, Frame, NewExtra ],
		write(IdentitySeq),nl,
		writeq(OutStream,NewTerm),
		write(OutStream,'.\n'),
		identity_markup_stream(InStream,OutStream)
	).

identity_markup_line([],[],[]).
identity_markup_line([C|Qs],[C|Hs],[1|Is]) :-
	identity_markup_line(Qs,Hs,Is).
identity_markup_line([Q|Qs],[H|Hs],[0|Is]) :-
	H \= Q,
	identity_markup_line(Qs,Hs,Is).

