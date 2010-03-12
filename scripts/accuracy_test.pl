:- ['../lost.pl'].
:- lost_include_api(interface).

test :-
	lost_sequence_file(gb_fragment,ReferenceFile),	
	lost_sequence_file(eg_fragment,PredictionFile),

	write('here'),

	get_annotation_file(accuracy_report,
			    [ReferenceFile,PredictionFile],
			    [
			     option(reference_functor,gb),
			     option(prediction_functor,eg),
			     option(start,1),
			     option(end,20000)
			    ],
			    OutputFile),

	write('Displaying output file: '),write(OutputFile),nl,
	readFile(OutputFile,Contents),
	atom_codes(Atom,Contents),
	write(Atom),
	nl.