:- ['../lost.pl'].

build_hmm_goals(Counter,GeneRefTerms,InStream,OutStream) :-
	Counter1 is Counter + 1,
	((0 is Counter1 mod 1000) -> write('procesed '), write(Counter1), write(' orfs'),nl ;	true),
	read(InStream,OrfTerm),
	((OrfTerm == end_of_file) ->
		true
	;
		OrfTerm =.. [_Functor,_SeqId,_Left,_Right,Dir,Frame,Extra],
		member(starts(StartsList), Extra),
		(member(stop([Stop]), Extra) -> % Some chunks do not have stops
			get_reference_starts(GeneRefTerms,Stop,Dir,Frame,CorrectStarts),
			findall(L,(member(Start,StartsList), length_in_codons(Start,Stop,L)), Lengths),
			lengths_annotation(StartsList,CorrectStarts,Annot1),
			fill_internal('-',Annot1,Annot2),
			write(OutStream,hmm_model(Lengths,Annot2)),
			write(OutStream,'.\n')
			;
			true % Just skip those
		),
		!,
		build_hmm_goals(Counter1,GeneRefTerms,InStream,OutStream)).

% Rewrite annnotations like [-,-,+,-,-] to [-,-,+,+,+]
fill_internal(_,[],[]).
fill_internal('+',[_|As],['+'|Bs]) :-
	fill_internal('+',As,Bs).
fill_internal('-',[A|As],[A|Bs]) :-
	fill_internal(A,As,Bs).

lengths_annotation([],_CorrectStarts,[]).
lengths_annotation([S|Ss],CorrectStarts,[A|As]) :-
	(member(S,CorrectStarts) ->
		A = '+'
		;
		A = '-'),
	lengths_annotation(Ss,CorrectStarts,As).

get_reference_starts(GeneRefTerms,Stop,Dir,Frame,Starts) :-
	findall(Start,stop_match(GeneRefTerms,Stop,Dir,Frame,Start),Starts).

stop_match(GeneRefTerms,Stop,Dir,Frame,Start) :-
	GeneRefTerms = [FirstTerm|_],
	FirstTerm =.. [GeneRefFunctor|_],
	((Dir=='+') ->
		MatchStop is Stop + 2,
		Matcher =.. [GeneRefFunctor,_,Start,MatchStop,Dir,Frame,_]
		;
		MatchStop is Stop - 2,
		Matcher =.. [GeneRefFunctor,_,MatchStop,Start,Dir,Frame,_]),
	member(Matcher,GeneRefTerms).

length_in_codons(Start,Stop,Codons) :-
	Codons is abs(Start - Stop) // 3.


test :-
	terms_from_file('/Users/cth/code/lost/data/refgenes.pl',GeneRefTerms),
	open('/Users/cth/code/lost/data/ecoli_all_orfs.seq',read,InStream),
	lost_tmp_directory(TmpDir),
	atom_concat(TmpDir,'/goals_tmp.pl',GoalsFile),
	open(GoalsFile,write,OutStream),
	build_hmm_goals(0,GeneRefTerms,InStream,OutStream),
	close(OutStream),
	close(InStream).
