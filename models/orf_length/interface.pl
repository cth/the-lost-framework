:- ['../../lost.pl'].
:- lost_include_api(interface).
:- lost_include_api(io).
:- lost_include_api(accuracy).
:- lost_include_api(viterbi_learn).

:- [smooth].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Simple length range model
% This model estimates a rough length distribution of coding ORFs. The length
% distribution is divided into ranges of subsequent lengths scores. 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% The goals used for learning. Takes a file with reference genes as input.
simple_range_model_learn([GeneRefFile],_,OutputFile) :-
	terms_from_file(GeneRefFile,GeneTerms),
	extract_gene_lengths(GeneTerms,GeneLengths),
	prism(length_model),
	['length_model.psm'],
	learn_gene_lengths(GeneLengths),
	save_sw(OutputFile).

% Perform annotation of ORFs using a trained model
simple_range_model_annotate([OrfFile,ParameterFile],_,OutputFile) :-
	prism(length_model),
	restore_sw(ParameterFile),
	['length_model.psm'],
	open(OrfFile,read,InStream),
	open(OutputFile,write,OutStream),
	simple_range_model_annotate_orf(0,InStream,OutStream),
	close(InStream),
	close(OutStream).

simple_range_model_annotate_orf(Counter,InStream,OutStream) :-
	Counter1 is Counter + 1,
	((0 is Counter1 mod 100) -> write('procesed '), write(Counter1), write(' orfs'),nl ;	true),
	read(InStream,ORF),
	((ORF == end_of_file) ->
		true
	;
		ORF =.. [Functor,SeqId,Left,Right,Dir,Frame,Extra],
		member(starts(StartsList), Extra),
		member(stop([Stop]), Extra),
		findall(L,(member(Start,StartsList), length_in_codons(Start,Stop,L)), Lengths),!,
		findall(R,(member(L,Lengths),get_range(L,R)),Ranges),!,
		findall(P,(member(R,Ranges),prob(msw_gene_length_range(R),P)),LengthScores),!,
		NewExtra = [length_ranges(Ranges),length_scores(LengthScores)|Extra],
		NewAnnot =.. [Functor,SeqId,Left,Right,Dir,Frame,NewExtra],
%		write(NewAnnot),
		write(OutStream,NewAnnot),
		write(OutStream,'\n'),
		!,
		simple_range_model_annotate_orf(Counter1,InStream,OutStream)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Simple length range model
% This model estimates a rough length distribution of coding ORFs. The length
% distribution is divided into ranges of subsequent lengths scores. 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% The goals used for learning. Takes a file with reference genes as input.
adph_model_learn([GeneRefFile],_,OutputFile) :-
			terms_from_file(GeneRefFile,GeneTerms),
			extract_gene_lengths(GeneTerms,GeneLengths),
			prism(length_model),
			['length_model.psm'],
			adph_learn_gene_lengths(GeneLengths),
			save_sw(OutputFile).



% The goals used for learning. Takes a file with reference genes as input.
adph2_model_learn([GeneRefFile],_,OutputFile) :-
			terms_from_file(GeneRefFile,GeneTerms),
			extract_gene_lengths(GeneTerms,GeneLengths),
			prism(length_model),
			['length_model.psm'],
			adph2_learn_gene_lengths(GeneLengths),
			save_sw(OutputFile).

% Perform annotation of ORFs using a trained model
adph_model_annotate([OrfFile,ParameterFile],_,OutputFile) :-
	prism(length_model),
	restore_sw(ParameterFile),
	['length_model.psm'],
	open(OrfFile,read,InStream),
	open(OutputFile,write,OutStream),
	simple_range_model_annotate_orf(0,InStream,OutStream),
	close(InStream),
	close(OutStream).

adph_model_annotate_orf(Counter,InStream,OutStream) :-
	Counter1 is Counter + 1,
	((0 is Counter1 mod 100) -> write('procesed '), write(Counter1), write(' orfs'),nl ;	true),
	read(InStream,ORF),
	((ORF == end_of_file) ->
		true
	;
		ORF =.. [Functor,SeqId,Left,Right,Dir,Frame,Extra],
		member(starts(StartsList), Extra),
		member(stop([Stop]), Extra),
		findall(L,(member(Start,StartsList), length_in_codons(Start,Stop,L)), Lengths),!,
		findall(P,(member(L,Lengths),prob(adph_model(L),P)),LengthScores),!,
		NewExtra = [length_scores(LengthScores)|Extra],
		NewAnnot =.. [Functor,SeqId,Left,Right,Dir,Frame,NewExtra],
%		write(NewAnnot),
		write(OutStream,NewAnnot),
		write(OutStream,'\n'),
		!,
		adph_model_annotate_orf(Counter1,InStream,OutStream)).

% Generate a file with probabilities of various lengths		
adph_test_probs([ParameterFile],_,OutputFile) :-
	prism(length_model),
	restore_sw(ParameterFile),
	['length_model.psm'],
	orf_lengths(Lengths),
	findall([L,P],(member(L,Lengths),prob(adph_model(L),P)),Distribution),
	tell(OutputFile),
	forall(member([L,P],Distribution),(write(L),write('\t'),write(P),nl)),
	told.
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Significance model:
% This model is trained using both positive and negative evidence
% - The positive evidence is used to built a gene length distribution
% - The negative evidence is used to build a non-gene length distribution
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Learning both gene range msw and nongene range msw

significance_model_learn([GeneRefFile,OrfFileRef],_,OutputFile) :-
	terms_from_file(GeneRefFile,GeneTerms),
	terms_from_file(OrfFileRef,NonGeneTerms),
	extract_gene_lengths(GeneTerms,GeneLengths),
	extract_gene_lengths(NonGeneTerms,NonGeneLengths),
	prism(length_model),
	['length_model.psm'],
	learn_gene_lengths(GeneLengths),
	learn_nongene_lengths(NonGeneLengths),
	learn_filter_probability(GeneTerms,NonGeneTerms),
	save_sw(OutputFile).
	
% There are two possible modes of prediction using the discriminative model
% 1. Start codon ranking 
%   - This approach takes ORFs/ORF-chunks as input and assigns each start
%     codon a significance score (a posterior probability defined as P(real-start)/P(fake-start))
% 2. Filtering 
%    - This approach takes Predictions in range format as input and basically
%      annotates as GENE if P(real-start) > P(fake-start)
%      annotates as NON-GENE if P(real-start) < P(fake-start)


% Method 1: Ranking
significance_model_rank_orf(Counter,InStream,OutStream) :-
	Counter1 is Counter + 1,
	((0 is Counter1 mod 1000) -> write('procesed '), write(Counter1), write(' orfs'),nl ;	true),
	read(InStream,ORF),
	((ORF == end_of_file) ->
		true
	;
		ORF =.. [Functor,SeqId,Left,Right,Dir,Frame,Extra],
		member(starts(StartsList), Extra),
		member(stop([Stop]), Extra),
		findall(L,(member(Start,StartsList), length_in_codons(Start,Stop,L)), Lengths),!,
		findall(R,(member(L,Lengths),get_range(L,R)),Ranges),!,
		findall(P,(member(R,Ranges),prob(msw_gene_length_range(R),P)),GeneLengthScores),!,
		findall(P,(member(R,Ranges),prob(msw_nongene_length_range(R),P)),NonGeneLengthScores),!,
		% Assuming log-probabilities
		calculate_significance_scores(GeneLengthScores,NonGeneLengthScores,SignificanceScores),
		NewExtra = [length_ranges(Ranges),length_scores_significance(SignificanceScores), length_scores(GeneLengthScores)|Extra],
		NewAnnot =.. [Functor,SeqId,Left,Right,Dir,Frame,NewExtra],
%		write(NewAnnot),
		write(OutStream,NewAnnot),
		write(OutStream,'\n'),
		!,
		significance_model_rank_orf(Counter1,InStream,OutStream)).

% utility, calculating of significance scores:
calculate_significance_scores([],[],[]).
calculate_significance_scores([A|As],[B|Bs],[C|Cs]) :-
	C is A - B,
	calculate_significance_scores(As,Bs,Cs).

% Method 2: Filtering

significance_model_filter([PredictionsFile,ParamFile],_Opts,OutputFile) :-
	prism(length_model),
	restore_sw(ParamFile),
	open(PredictionsFile,read,InStream),
	open(OutputFile,write,OutStream),
	read_and_annotate_predictions(InStream,OutStream),
	close(InStream),
	close(OutStream).

significance_model_filter_prediction(Counter,InStream,OutStream) :-
	Counter1 is Counter + 1,
	((0 is Counter1 mod 1000) -> write('procesed '), write(Counter1), write(' predictions'),nl ;	true),
	read(InStream,Prediction),
	((Prediction == end_of_file) ->
		true
	;
		Prediction =.. [Functor,SeqId,Left,Right,Dir,Frame,Extra],
		length_in_codons(Left,Right,Length),
		discriminative_model(Length,Coding),
		((Coding='+') ->
			% Also, add the score to the Extra list.
			get_range(Length,Range),
			prob(msw_gene_length_range(Range),Prob),
			NewExtra = [length_range(Range),length_score(Prob)|Extra],
			NewAnnot =.. [Functor,SeqId,Left,Right,Dir,Frame,NewExtra],
			write(OutStream,NewAnnot),
			write(OutStream,'.\n')
			;
			true
		)
	),
	!,
	significance_model_filter_prediction(Counter1,InStream,OutStream).

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
