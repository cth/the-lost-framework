:- ['../../../lost.pl'].
:- lost_include_api(misc_utils).

run_split1 :-
	lost_data_directory(DataDir),
	% Create file names:
%	atom_integer(CategoriesAtom,Categories),
	%atom_concat_list([DataDir,'gf_',CategoriesAtom,'_predictions.pl'],PredictFile),
	%atom_concat_list([DataDir,'gf_',CategoriesAtom,'_accuracy.txt'],AnnotationFile),
	write('PredictionsFile:'), write(PredictFile), nl,
	write('AnnnotationsFile:'), write(AnnotationFile),nl,
	% input files:
	verified_file(GF),
	predictions_file(PF),
	% Run the genome filter:
	run_model(genome_filter,
		split_annotate([GF,PF],[debug(true),score_categories(40),score_functor(score)],SelectedPredictionsFile)),
	write('Wrote selected predictions to: '),
	write(SelectedPredictionsFile),nl,
	run_model(accuracy_report, annotate([GF,SelectedPredictionsFile],[start(1),end(max)],AccuracyFile)),
	write('Accuracy report is : '), write(AccuracyFile), nl,
	readFile(AccuracyFile,Contents),
	atom_codes(Atom,Contents),
	write(Atom),nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Turn off gene-finder signal (e.g. all scores are considered 
% to be the same.)
% This is easily done by making just one score group
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

turnoff_experiment :-
	run(1).
	% Doesn't work well because of high delete probability (e.g. delete state dominates!)
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Experiment: Different delete state probabilities
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

t1 :- 
	run_gf(40,0.3,AccuracyFile),
		write(AccuracyFile),nl.

tx_split(P) :- 	
	run_gf(true,60,P,AccuracyFile),
	writeq(experiment_result_index(60,P,AccuracyFile)),write('.'),nl,
	write('txmap: '),write('P='),write(P),write(AccuracyFile),nl,
	write(AccuracyFile),nl.
	
tx_nosplit(P) :- 	
	run_gf(false,60,P,AccuracyFile),
	writeq(experiment_result_index(60,P,AccuracyFile)),write('.'),nl,
	write('txmap: '),write('P='),write(P),write(AccuracyFile),nl,
	write(AccuracyFile),nl.	


exp_del_state_prob :-
	DeleteProbs = [0.7,0.8, 0.9], %, 0.82, 0.83, 0.84, 0.85, 0.86, 0.87, 0.88, 0.89, 0.9, 0.91, 0.92, 0.93, 0.94, 0.95, 0.96, 0.97, 0.98, 0.99],
	findall(File,(member(P,DeleteProbs),run_gf(40,P,File)), Files),
	zip(DeleteProbs,Files,Both),
	write('File list:'),nl,
	forall(member([P,F],Both), (write(P),write('\t'),write(F),write('\n'))).
	
run_delete_state_probability_experiment1 :-
	StartProb = 0.1,
	MaxProb = 0.9,
	ScoreGroups = 40,
	delete_state_probability_experiment(ScoreGroups,StartProb,0.2,MaxProb,ResultFiles),
	terms_to_file(ResultFiles,'delete_state_probability_experiment1_result_files.pl'),
	% Create tab separated file for easy input to R,
	write('ScoreGroups\tDeleteProbability\tStopsCorrect\tStopWrong\tStopSensitivity\sStopSpecificity'),
	delete_state_probability_experiment1_report(ResultFiles).
	
delete_state_probability_experiment1_report([]).
delete_state_probability_experiment1_report([experiment_result(P,SGs,AccFile)|Rest]) :-
	terms_from_file(AccFile,Terms),
	member(accuracy_report(correct_stops,CorrectStops),Terms),
	member(accuracy_report(wrong_stops,CorrectStops),Terms),
	member(accuracy_report(gene_stop_sensitivity,SN),Terms),
	member(accuracy_report(gene_stop_specificity,SP),Terms),
	write(SGs),	write('\t'),
	write(P), write('\t'),
	write(CorrectStops),write('\t'),
	write(WrongStops),write('\t'),
	write(SN), write('\t'),
	write(SP),write('\n'),
	delete_state_probability_experiment1_report(Rest).
	

delete_state_probability_experiment(_, Probability,_,MaxProb,[]) :-
	Probability >= MaxProb.
delete_state_probability_experiment(ScoreGroups, Probability,Increment,MaxProb,[experiment_result(Probability,ScoreGroups,AccuracyFile)|Rest]):-
	run_gf(ScoreGroups,Probability,AccuracyFile),
	NextProbability is Probability + Increment,
	delete_state_probability_experiment(ScoreGroups,NextProbability,Increment,MaxProb,Rest).
	
	
%%%%%

runall :-
        ScoreSymbols = [ 25, 50, 75, 100 ],
        forall(member(X,ScoreSymbols),run_gf(X)).

run_gf(SplitGenome,Categories,OutFile) :-
	run_gf(SplitGenome,Categories,false,OutFile).
	        
run_gf(SplitGenome,Categories,DeleteProb,AccuracyFile) :- 
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
		((SplitGenome==true) ->
			run_model(genome_filter,
        		split_annotate([GF,PF],[terminus(1588800),origin(3923500),debug(true),score_categories(Categories),score_functor(score),override_delete_probability(DeleteProb)],SelectedPredictionsFile))
				% Note, ter and ori is hardcoded for e-coli here
			;
			run_model(genome_filter,
        		annotate([GF,PF],[debug(true),score_categories(Categories),score_functor(score),override_delete_probability(DeleteProb)],SelectedPredictionsFile))
		),
        write('Wrote selected predictions to: '),
        write(SelectedPredictionsFile),nl,
        run_model(accuracy_report, annotate([GF,SelectedPredictionsFile],[start(1),end(max)],AccuracyFile)),
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



