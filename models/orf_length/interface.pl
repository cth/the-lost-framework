:- ['../../lost.pl'].
:- lost_include_api(interface).
:- lost_include_api(io).
:- lost_include_api(accuracy).
:- lost_include_api(viterbi_learn).

:- [smooth].

annotate([OrfFile,ParamFile],_Opts,OutputFile) :-
	

learn([GeneRefFile],_,OutputFile) :-
	terms_from_file(GeneRefFile,GeneTerms),
	extract_gene_lengths(GeneTerms,GeneLengths),
	prism(length_model),
	['length_model.psm'],
	random_split_list(GeneLengths,RandomHalf,_),
	learn_lengths(RandomHalf),
	save_sw(OutputFile),
	!,
	terms_from_file(OutputFile,[switch(length_range,_,Outcomes,Probs)]),
	tell('/tmp/range_probs_20_vb_xval3.dat'),
	write_data(Outcomes,Probs),
	told.
	
write_date([],[]).
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


%% Utility predicates:

