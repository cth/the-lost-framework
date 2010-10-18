:- ['../lost.pl'].
:- lost_include_api(interface).
:- lost_include_api(utils_parser_report).
:- lost_include_api(prism_parallel).
:- lost_include_api(viterbi_learn).
:- [script_parser].
:- [filter_genes].


genbank_database_file(PTTName,DatabaseFile) :-
	lost_sequence_file(PTTName,PTTFile),
	run_model(parser_ptt,annotate([PTTFile],[],DatabaseFile)).

easygene_database_file(EgName,DatabaseFile) :-
	lost_sequence_file(EgName,InputFile),
        run_model(parser_easygene,annotate([InputFile],[],DatabaseFile)).

consorf_prediction_file(F) :-
        lost_sequence_file(consorf_pred,F).

testhard :-
        lost_sequence_file('gene_filter_+1',Ref),
        lost_sequence_file('lost_genefinder_hard', Pred),
        run_model(accuracy_report,annotate([Ref,Pred],[start(1),end(1543932)],OutputFile)),
	write('Displaying output file: '),write(OutputFile),nl,
	readFile(OutputFile,Contents),
	atom_codes(Atom,Contents),
	write(Atom),
	nl.


test :-
	genbank_database_file('U00096_ptt',ReferenceFile),
	lost_sequence_file('consorf_U00096',PredictionFile),
	% OldOptions = [reference_functor(gb),prediction_functor(eg),start(1),end(1700000)],
	writeq(run_model(accuracy_report, annotate([ReferenceFile,PredictionFile], [start(1)],OutputFile))),nl,
	run_model(accuracy_report, annotate([ReferenceFile,PredictionFile], [start(1)],OutputFile)),
	write('Displaying output file: '),write(OutputFile),nl,
	readFile(OutputFile,Contents),
	atom_codes(Atom,Contents),
	write(Atom),
	nl.        

test2 :-
	lost_sequence_file('U00096',Genome),
       lost_sequence_file('U00096_ptt',PTT),
	lost_sequence_file('consorf_U00096',PredictionFile),
	parser_ptt(PTT,Genes),

        % Filter uncertain genes
       write('Filtering genes which from the description seem uncertain:'),nl,
       filter_uncertain_genes(Genes,Genome,[],Certain),
       write('Results written to file: '), write(Certain), nl,

        % Filter y-genes
       write('Filtering genes which from the description seem uncertain:'),nl,
       filter_uncertain_genes(Genes,Genome,[],Certain),
       write('Results written to file: '), write(Certain), nl,

	run_model(accuracy_report, annotate([Certain,PredictionFile], [start(1)],OutputFile)),
	write('Displaying output file: '),write(OutputFile),nl,
	readFile(OutputFile,Contents),
	atom_codes(Atom,Contents),
	write(Atom),
	nl.

test3(Dir,Frame,PredictionFile) :-
	lost_sequence_file('U00096',Genome),
       lost_sequence_file('U00096_ptt',PTT),
	lost_sequence_file(PredictionFile,Predictions),
	parser_ptt(PTT,Genes),

        % Filter uncertain genes
       write('Filtering genes which from the description seem uncertain:'),nl,
       filter_uncertain_genes(Genes,Genome,[],Certain1),
       write('Results written to file: '), write(Certain1), nl,

        % Filter y-genes
       write('Filtering y-genes:'),nl,
       filter_y_genes(Certain1,Genome,Certain2),
       write('Results written to file: '), write(Certain2), nl,

        % Filter non dirframe genes
       write('Filtering genes which from different strand/frames:'),nl,
       filter_non_dir_frame_genes(Dir,Frame,Certain2,Genome,InDirframe),
       write('Results written to file: '), write(InDirframe), nl,

	run_model(accuracy_report, annotate([InDirframe,Predictions], [start(1)],OutputFile)),
	write('Displaying output file: '),write(OutputFile),nl,
	readFile(OutputFile,Contents),
	atom_codes(Atom,Contents),
	write(Atom),
	nl.






script_acc(ReferenceFile,PredictionFile,Max) :-
        lost_sequence_file(ReferenceFile,ReferenceFile2),
        lost_sequence_file(PredictionFile,PredictionFile2),
        run_model(accuracy_report, annotate([ReferenceFile2,PredictionFile2], [start(1),end(Max)],OutputFile)),
	write('Displaying output file: '),write(OutputFile),nl,
	readFile(OutputFile,Contents),
	atom_codes(Atom,Contents),
	write(Atom),
	nl.
