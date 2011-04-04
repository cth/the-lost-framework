:- ['../../lost.pl'].
:- lost_include_api(interface).
:- lost_include_api(io).
:- lost_include_api(accuracy).
:- lost_include_api(viterbi_learn).

:- [smooth].

% OrfFile format:
annotate([OrfFile,ParamFile],_Opts,OutputFile) :-
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
		findall(P,(member(R,Ranges),prob(msw_length_range(R),P)),LengthScores),!,
		NewExtra = [length_ranges(Ranges),length_scores(LengthScores)|Extra],
		NewAnnot =.. [Functor,SeqId,Left,Right,Dir,Frame,NewExtra],
%		write(NewAnnot),
		write(OutStream,NewAnnot),
		write(OutStream,'\n'),
		!,
		read_and_annotate_orf(InStream,OutStream)).
		
length_in_codons(Start,Stop,Codons) :-
	Codons is abs(Start - Stop) // 3.
	
learn([GeneRefFile],_,OutputFile) :-
	terms_from_file(GeneRefFile,GeneTerms),
	extract_gene_lengths(GeneTerms,GeneLengths),
	prism(length_model),
	['length_model.psm'],
	random_split_list(GeneLengths,RandomHalf,_),
	learn_lengths(RandomHalf),
	learn_lengths(GeneLengths),
	save_sw(OutputFile),
	!,
	terms_from_file(OutputFile,[switch(length_range,_,Outcomes,Probs)]),
	tell('/tmp/range_probs_salmonella_250_vb.dat'),
	write_data(Outcomes,Probs),
	told.
	


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
