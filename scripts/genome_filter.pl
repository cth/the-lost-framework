:- ['../lost.pl'].
:- lost_include_api(misc_utils).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Turn off gene-finder signal (e.g. all scores are considered 
% to be the same.)
% This is easily done by making just one score group
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

turnoff_experiment :-
	run(1).

%%%%%%%% Experiment r1: veco_cyc and Soerens gene finder %%%%%%


runall :-
        ScoreSymbols = [ 25, 50, 75, 100 ],
        forall(member(X,ScoreSymbols),run(X)).
        
run(Categories) :- 
        lost_data_directory(DataDir),
        % Create file names:
        atom_integer(CategoriesAtom,Categories), 
        %atom_concat_list([DataDir,'gf_',CategoriesAtom,'_predictions.pl'],PredictFile),
        %atom_concat_list([DataDir,'gf_',CategoriesAtom,'_accuracy.txt'],AnnotationFile),
        write('PredictionsFile:'), write(PredictFile), nl,
        write('AnnnotationsFile:'), write(AnnotationFile),nl,
        % input files:
		verified_file(GF),
		predictions_file(PF),
        % Run the genome filter:
		run_model(genome_filter,
        	annotate([GF,PF],[score_categories(Categories),score_functor(score)],SelectedPredictionsFile)),
        write('Wrote selected predictions to: '),
        write(SelectedPredictionsFile),nl,
        %move_data_file(SelectedPredictionsFile,PredictFile),
        % Accuracy report:
        run_model(accuracy_report, annotate([GF,PredictFile],[start(1),end(max)],AccuracyFileTmp)),
        write('Accuracy report is : '), write(AccuracyFile), nl,
		readFile(AccuracyFile,Contents),
		atom_codes(Atom,Contents),
		write(Atom),nl.

verified_file(GeneFileProlog) :-
    lost_data_file('nc000913_2_vecocyc_ptt',PttFile),
    run_model(parser_ptt,annotate([PttFile],[],GeneFileProlog)).

predictions_file(PredictionsFile) :-
        lost_data_file('nc000913_2_all_urfs_for_real.pl',PredictionsFile).

tt1 :-
	verified_file(F),
	terms_from_file(F,Ts),
	write(Ts),nl,
	map(calc_len,Ts,Ls),
	write(Ls).
% where
  calc_len([],[]).
  calc_len(T,Length) :- T =.. [_,_,L,R|_], L > R, !,  Length is L - R.
  calc_len(T,Length) :-  T =.. [_,_,L,R|_], Length is R - L.

%predictions_file('data/100k_all_urfs.pl').
%predictions_file('data/nc000913_2_FN0_mm_3.pl').

learn_steps :-
    verified_file(RefGeneFile),
    terms_from_file(RefGeneFile,RefGenes),
    predictions_file(PFile),
    terms_from_file(PFile,Predictions1),
    elements_with_functor(prediction,Predictions1,Predictions),
    length(RefGenes,RGL),
    write('number of reference genes: '), write(RGL), nl, 
    length(Predictions,PL),
    write('number of predictions: '), write(PL), nl,
    split_predictions(Predictions,RefGenes,Correct,Incorrect),
    length(Correct,CorrectLen),
    write('correct predictions: '), write(CorrectLen),nl,
%	write(Correct),nl,
    length(Incorrect,IncorrectLen),
    write('Incorrect predictions: '), write(IncorrectLen),nl,
    terms_to_file('incorrect.txt',Incorrect),
    terms_to_file('correct.txt',Correct).
	
	
learn :-
	verified_file(GF),
	predictions_file(PF),
	file_functor(PF,PFFunctor),
	write('prediction functor: '), write(PFFunctor), nl,
	learn([GF,PF],[score_functor(score)],_),
	save_sw.

learn2 :-
	verified_file(GF),
	predictions_file(PF),
	file_functor(PF,PFFunctor),
	write('prediction functor: '), write(PFFunctor), nl,
	learn2([GF,PF],[score_functor(score)],_),
	save_sw.

decode :-
	verified_file(GF),
	predictions_file(PF),
	annotate([GF,PF],[score_functor(score)],'predictions.pl').

maxdist :-
       	verified_file(GF),
	predictions_file(PF),
        terms_from_file(GF,GoldenUnsort),
        terms_from_file(PF,PredictionsUnsort),
        sort(GoldenUnsort,TPs),
        sort(PredictionsUnsort,Predictions),
        max_tp_distance(TPs,Predictions,0,MaxDist),
        write('The maximal distance is: '),
        write(MaxDist),nl.



