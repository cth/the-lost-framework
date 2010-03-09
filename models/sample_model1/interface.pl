:- ['../../lost.pl'].
:- lost_include_api(interface).

% This is what is used to get the best annotation
lost_best_annotation(ParamFile,[InputFile],ExtraOptions,OutputFile) :-
	write('sample model 1: '),nl,
	write(lost_best_annotation(ParamFile,[InputFile],ExtraOptions,OutputFile)),nl,
	prism(sample1), % Load the actual PRISM model
	write(restore_sw(ParamFile)),nl, % Restore switch values
	restore_sw(ParamFile), % Restore switch values
	load_sequence_list_from_file(InputFile,InputSequence), % Load the input sequence
        % Derive an annotation somehow
        write(viterbig(sample1(InputSequence,OutputSequence))),nl,
	check_or_fail(viterbig(sample1(InputSequence,OutputSequence)), 
                        error(viterbig_says_no_no_no)),
        write('model 1 produced output sequence: '),
        write(OutputSequence),nl,
	write(save_sequence_list_to_file(OutputFile,OutputSequence)),
	save_sequence_list_to_file(OutputFile,OutputSequence), % Save result to filename given
	write('model 1 terminated successfully.'),nl.
