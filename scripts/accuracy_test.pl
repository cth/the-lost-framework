:- ['../lost.pl'].
:- lost_include_api(interface).

genbank_database_file(PTTName,DatabaseFile) :-
	lost_sequence_file(PTTName,PTTFile),
	get_annotation_file(parser_ptt,[PTTFile],[],DatabaseFile).

easygene_database_file(EgName,DatabaseFile) :-
	lost_sequence_file(EgName,InputFile),
        get_annotation_file(parser_easygene,[InputFile],[],DatabaseFile).

test :-
	genbank_database_file('U00096_ptt',ReferenceFile),
	easygene_database_file('U00096_ptt',PredictionFile),

	get_annotation_file(accuracy_report,
			    [ReferenceFile,PredictionFile],
			    [
			     option(reference_functor,gb),
			     option(prediction_functor,eg),
			     option(start,1),
			     option(end,100000)
			    ],
			    OutputFile),

	write('Displaying output file: '),write(OutputFile),nl,
	readFile(OutputFile,Contents),
	atom_codes(Atom,Contents),
	write(Atom),
	nl.
