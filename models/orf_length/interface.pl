:- ['../../lost.pl'].
:- lost_include_api(interface).
:- lost_include_api(io).
:- lost_include_api(accuracy).
:- lost_include_api(viterbi_learn).

:- [smooth].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Prediction using a HMM-type model for ORF chunks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
annotate_orf_chunks([OrfFile,ParamFile],_Opts,OutputFile) :-
	prism(length_model),
	restore_sw(ParamFile),
	open(OrfFile,read,InStream),
	open(OutputFile,write,OutStream),
	read_and_annotate_orf(InStream,OutStream),
	close(InStream),
	close(OutStream).

read_and_annotate_orf(InStream,OutStream) :-
	write('.'),
	read(InStream,ORF),
	((ORF == end_of_file) ->
		true
	;
		ORF =.. [Functor,SeqId,Left,Right,Dir,Frame,Extra],
		member(starts(StartsList), Extra),
		member(stop([Stop]), Extra),
		findall(L,(member(Start,StartsList), length_in_codons(Start,Stop,L)), Lengths),!,
		findall(R,(member(L,Lengths),get_range(L,R)),Ranges),!,
		viterbi(hmm_model(Lengths,Annotations)),
		findall(P,(member(R,Ranges),prob(msw_length_range(R),P)),LengthScores),!,
		NewExtra = [length_ranges(Ranges),length_scores(LengthScores)|Extra],
		NewExtra = [start_annotations(Annotations),length_scores(LengthScores)|Extra],
		NewAnnot =.. [Functor,SeqId,Left,Right,Dir,Frame,NewExtra],
%		write(NewAnnot),
		write(OutStream,NewAnnot),
		write(OutStream,'.\n'),
		!,
		read_and_annotate_orf(InStream,OutStream)).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Learning a HMM-type model from ORF chunks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

learn_hmm_from_orfs([GeneRefFile,OrfFileRef],_,OutputFile) :-
	terms_from_file(GeneRefFile,GeneRefTerms),
	open(OrfFileRef,read,InStream),
	lost_tmp_directory(TmpDir),
	atom_concat(TmpDir,'/learn_length_hmm_from_orfs.pl',GoalsFile),
	open(GoalsFile,write,OutStream),
	build_hmm_goals(0,GeneRefTerms,InStream,OutStream),
	close(OutStream),
	close(InStream),
	terms_from_file(GoalsFile,Goals),
	learn(Goals),
	save_sw(OutputFile).

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


%%%%%%%%%%%%%%%%%%% End learn goals %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
learn_filter_probability(GeneTerms,PredictionTerms) :-
	range_match_terms(GeneTerms,GeneMatchTerms),
	range_match_terms(PredictionTerms,PredictionMatchTerms),
	intersection(GeneMatchTerms,PredictionMatchTerms,Intersection),
	length(PredictionTerms,NumPredictionTerms),
	length(Intersection,NumMatching),
	
	set_sw()
*/	



length_in_codons(Start,Stop,Codons) :-
	Codons is abs(Start - Stop) // 3.	
	
%% Some utility predicates:
	
write_data([],[]).
write_data([O|Os],[P|Ps]) :-
	write(O),
	write('\t'),
	write(P),
	nl,
	write_data(Os,Ps).

extract_gene_lengths([],[]).
extract_gene_lengths([G|Gs],[L|Ls]) :-
	G =.. [_Func,_Id,Start,End,_Strand,_Frame,_extra],
	NucleotideLength is abs(Start - End),
	L is NucleotideLength // 3,
	extract_gene_lengths(Gs,Ls).

% For x-validation experiment
random_split_list([],[],[]).	
random_split_list([L|Ls],FirstHalf,SecondHalf) :-
	random_uniform(1.0,X),
	((X > 0.5) ->
		FirstHalf = [L|Fs],
		SecondHalf = Ss
		;
		FirstHalf = Fs,
		SecondHalf = [L|Ss]),
	random_split_list(Ls,Fs,Ss).
	
%orf_to_range_facts(ORF,RangeFacts) :-
%	ORF =.. [_Functor,SeqId,_Left,_Right,Dir,Frame,Extra],
%	member(starts(Starts), Extra),
%	member(stop([Stop]), Extra),
%	findall(RangeFact,(	member(Start,Starts),
%						RangeFact=range_fact(SeqId,Start,Stop,Dir,Frame,[])),
%			RangeFacts).
