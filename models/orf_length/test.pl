min_score_categories(0).
max_score_categories(99).

upto(StartEnd,StartEnd,[StartEnd]).
upto(Start,End,[Start|Rest]) :-
	NewStart is Start + 1,
	upto(NewStart,End,Rest).
	
divide_list([],_,[]).

divide_list(L,0,L).

divide_list(List,NumChunks,[Chunk|ChunksRest]) :-
	length(List,ListLen),
	ChunkLen is ListLen // NumChunks,
	first_n(ChunkLen,List,Chunk),
	append(Chunk,RestList,List),
	RestNumChunks is NumChunks - 1,
	divide_list(RestList,RestNumChunks,ChunksRest).
	
first_n(_,[],[]).
first_n(0,_,[]).
first_n(N,[E|R1],[E|R2]) :-
	N1 is N - 1,
	first_n(N1,R1,R2).

sequence_range(Seq,[First,Last]),
	reverse(Seq,RevSeq),
	Seq = [First|_],
	RevSeq = [Last|_].
	
test_dl :-
	divide_list([1,2,3,4,5,6,7,8,9,10],4,Chunks),
	write(Chunks),nl.
	
length_to_range(Len,Range) :-
	score_categories(NumCategories),
	gene_lengths(GeneLengths),
	divide_list(GeneLengths,NumCategories,SubSequences),
	member(Seq,Divided),
	sequence_range(Seq,Range),
	Range = [RangeMin,RangeMax],
	Len >= RangeMin,
	Len <= RangeMax.
	
%%%%
% A scratch pad for stupid ideas
%%%% 

get_length_score(Length,Score) :-
	values(gene_length,Values),
	get_distribution(gene_length,Probs),
	accumulate_smaller(Length, Values, Probs,0,SumProbAfter),
	values(score_categories,ScoreValues),
	get_distribution(score_categories,ScoreProbs),
	lookup_category(SumProbAfter,ScoreValues,ScoreProbs,0,Score).

range_prob(StartLen,EndLen,RangeProb) :-
	values(gene_length,Values),
	findall(L,(member(L,Values),L<=StartLen,L>=EndLen), Lengths),
	findall(P,(member(O,Lengths),SwitchGoal=..[SwitchName,O],prob(SwitchGoal,P)),Probs),
	sumlist(Probs,RangeProb).
	
outcome_ranges(NumRanges,Ranges) :-
	values(gene_length,Outcomes),
	divide_list(OutComes,NumRanges,Divided),
	findall(Range,(member(Seq,DivideD),sequence_range(Seq,Range)),Ranges).
	
	
%%%


lookup_category(TargetProb, [_|Vs],[P|Ps],AccProb,TargetCategory) :-
	NewAccProb is AccProb + P,
	NewAccProb < TargetProb,
	lookup_category(TargetProb,Vs,Ps,NewAccProb,TargetCategory).

lookup_category(TargetProb, [V|_], _, AccProb, V) :-
	AccProb =< TargetProb.
	
accumulate_smaller(V, [V|_],[P|_],SumProbBefore,SumProbAfter) :-
	SumProbAfter is SumProbBefore + P.

accumulate_smaller(TargetValue, [V|Vs],[P|Ps],SumProbBefore,SumProbAfter) :-
	TargetValue \= V,
	NewSumProbBefore is SumProbBefore + P,
	accumulate_smaller(TargetValue, Vs,Ps,NewSumProbBefore,SumProbAfter).


%%%

/*	First version
learn([GeneRefFile],_,OutputFile) :-
	terms_from_file(GeneRefFile,GeneTerms),
	extract_gene_lengths(GeneTerms,GeneLengths),
%	write(GeneLengths),nl,
	length(GeneLengths,GeneLengthsLength),
	write(GeneLengthsLength),nl,
	eliminate_duplicate(GeneLengths,GeneLengthsSet),
	length(GeneLengthsSet,SetLen),
	write(SetLen),nl,
	list_add_functor(gene_length,GeneLengths,TrainingGoals),
%	write(TrainingGoals),nl,
	length(TrainingGoals,TrainingGoalsLength),
	write(TrainingGoalsLength),nl,	
	prism(length_model),
	set_sw_all,
	learn(TrainingGoals),
	['length_model.psm'],
	smooth_msw(5,gene_length,[0.25,0.5,1,0.5,0.25]),
	save_sw(OutputFile).
*/


%%%%

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
		findall(P,(member(R,Ranges),prob(msw_length_range(R),P)),LengthScores),!,
		NewExtra = [length_ranges(Ranges),length_scores(LengthScores)|Extra],
		NewAnnot =.. [Functor,SeqId,Left,Right,Dir,Frame,NewExtra],
%		write(NewAnnot),
		write(OutStream,NewAnnot),
		write(OutStream,'\n'),
		!,
		read_and_annotate_orf(InStream,OutStream)).
		
		
%%%%

read_and_annotate_prediction(InStream,OutStream) :-
	write('.'),
	read(InStream,Prediction),
	((Prediction == end_of_file) ->
		true
	;
		Prediction =.. [Functor,SeqId,Left,Right,Dir,Frame,Extra],
		member(starts(StartsList), Extra),
		member(stop([Stop]), Extra),
		findall(L,(member(Start,StartsList), length_in_codons(Start,Stop,L)), Lengths),!,
		findall(R,(member(L,Lengths),get_range(L,R)),Ranges),!,
		findall(P,(member(R,Ranges),prob(msw_length_range(R),P)),LengthScores),!,
		get_range(),
		
		NewExtra = [length_ranges(Ranges),length_scores(LengthScores)|Extra],
		NewAnnot =.. [Functor,SeqId,Left,Right,Dir,Frame,NewExtra],
%		write(NewAnnot),
		write(OutStream,NewAnnot),
		write(OutStream,'\n'),
		!,
		read_and_annotate_orf(InStream,OutStream)).





% Learns only the gene range msw
learn([GeneRefFile],_,OutputFile) :-
	terms_from_file(GeneRefFile,GeneTerms),
	extract_gene_lengths(GeneTerms,GeneLengths),
	prism(length_model),
	['length_model.psm'],
	%	random_split_list(GeneLengths,RandomHalf,_),
	%	learn_lengths(RandomHalf),
	learn_lengths(GeneLengths),
	save_sw(OutputFile),
	!,
	terms_from_file(OutputFile,[switch(length_range,_,Outcomes,Probs)]),
	tell('/tmp/range_probs_salmonella_250_vb.dat'),
	write_data(Outcomes,Probs),
	told.

	% Predictions format
	annotate([PredictionsFile,ParamFile],_Opts,OutputFile) :-
		prism(length_model),
		restore_sw(ParamFile),
		open(PredictionsFile,read,InStream),
		open(OutputFile,write,OutStream),
		read_and_annotate_predictions(InStream,OutStream),
		close(InStream),
		close(OutStream).
		
		range_match_terms([],[]).
		range_match_terms([T|Ts],[MT|MTs]) :-
			T =.. [Functor,SeqId,Left,Right,Dir,Frame,Extra],
			MT =.. [match,Left,Right,Dir,Frame].
	