:- ['../../lost.pl'].
:- use(genedb).
:- use(lists).

annotate_orfs(OrfsFile,OutputFile) :-
	terms_from_file(OrfsFile,Orfs),
	annotate_with_amber_codons(Orfs,AnnotOrfs),
	terms_to_file(OutputFile,AnnotOrfs).
	
annotate_with_amber_codons([],[]).

annotate_with_amber_codons([Orf|Rest],[UpdatedOrf|UpdatedRest]) :-
	gene_extra_field(Orf,sequence,Sequence),
	uag_positions(1,Sequence,RelativePositions),
	calculate_absolute_positions(Orf,RelativePositions,AbsolutePositions),
	gene_add_extra_field(Orf,in_frame_stops,AbsolutePositions,UpdatedOrf),
	annotate_with_amber_codons(Rest,UpdatedRest).

% must 
uag_positions(_,[],[]).

uag_positions(Pos,[t,a,g|SeqRest],[Pos|RestMatch]) :-
	!,
	NextPos is Pos + 3,
	uag_positions(NextPos,SeqRest,RestMatch).
	
uag_positions(Pos,[_,_,_|SeqRest],RestMatch) :-
	NextPos is Pos + 3,
	uag_positions(NextPos,SeqRest,RestMatch).

calculate_absolute_positions(_Orf,[],[]).

calculate_absolute_positions(Orf,[P|Ps],[R|Rs]) :-
	gene_strand(Orf,'+'),
	gene_left(Orf,Left),
	R is Left + P - 1,
	calculate_absolute_positions(Orf,Ps,Rs).	

calculate_absolute_positions(Orf,[P|Ps],[R|Rs]) :-
	gene_strand(Orf,'-'),
	gene_right(Orf,Right),
	R is 1 + (Right - P),
	calculate_absolute_positions(Orf,Ps,Rs).
	
	
test1 :-
	Orf=orf(na,26,85,'-',3,[length(60),stop(28),starts([85,82,67,58,49,40,34]),in_frame_stops([64]),sequence([a,t,g,a,t,c,a,g,t,c,a,t,a,g,t,g,t,t,a,t,a,t,a,g,g,c,t,a,t,t,t,c,c,t,c,t,a,t,a,a,g,g,g,g,g,a,t,g,t,t,c,a,t,t,t,t,a,t,a,a])]),
	gene_extra_field(Orf,sequence,Seq),
	uag_positions(1,Seq,Matches),
	writeln(Matches),
	calculate_absolute_positions(Orf,Matches,AbsoluteMatches),
	writeln(AbsoluteMatches),
	AbsoluteMatches=[64].
	
test2 :-
	Orf=orf(na,678,821,'+',1,[length(144),stop(819),starts([678,690,693,777,798,804,810]),in_frame_stops([738]),sequence([a,t,a,c,t,t,g,a,a,a,c,c,a,t,c,a,t,t,a,a,t,t,a,t,t,t,c,t,a,t,t,g,t,c,t,t,a,g,g,a,a,a,a,g,t,t,a,t,t,c,c,t,t,t,c,a,g,t,c,t,t,a,g,a,c,a,t,t,c,a,g,a,t,a,t,t,t,t,a,a,a,c,c,a,a,c,c,t,a,c,t,c,a,t,t,c,t,t,t,a,t,t,t,g,c,a,g,a,a,a,t,a,g,g,a,a,a,a,a,c,a,t,t,a,a,a,a,t,a,t,c,a,a,t,c,t,a,t,t,t,t,t,g,a])]),
	gene_extra_field(Orf,sequence,Seq),
	uag_positions(1,Seq,Matches),
	writeln(Matches),
	calculate_absolute_positions(Orf,Matches,AbsoluteMatches),
	writeln(AbsoluteMatches),
	AbsoluteMatches=[738].
