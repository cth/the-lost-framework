:- ['../lost.pl'].
:- lost_include_api(interface).

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
	easygene_database_file('eg_U00096',PredictionFile),
	OldOptions = [reference_functor(gb),prediction_functor(eg),start(1),end(1700000)],
	run_model(accuracy_report, annotate([ReferenceFile,PredictionFile], [start(1),end(1700000)],OutputFile)),
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
