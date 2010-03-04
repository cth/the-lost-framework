:- ['../../lost.pl'].
:- lost_include_api(interface). 

% This is what is used to get the best annotation
lost_best_annotation(ParamFile,[InputFile],OutputFile) :-
	write('sample model 1: '),nl,
	write(lost_best_annotation(ParamFile,[InputFile],OutputFile)),nl,
	prism(sample1), % Load the actual PRISM model
	restore_sw(ParamFile), % Restore switch values
	load_sequence_list_from_file(InputFile,InputSequence), % Load the input sequence
	viterbig(sample1(InputSequence,OutputSequence)), % Derive an annotation somehow
	save_sequence_list_to_file(OutputFile,OutputSequence). % Save result to filename given
