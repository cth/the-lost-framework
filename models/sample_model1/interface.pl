:- ['../../lost.pl'].
:- lost_include_api(interface).

% This is what is used to get the best annotation
lost_best_annotation([InputFile],Options,OutputFile) :-
	write('sample model 1: '),nl,
	write(lost_best_annotation(ParamFile,[InputFile],Options,OutputFile)),nl,
	lost_required_option(Options,parameter_file,ParamFile),
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
	write(save_sequence_list_to_file(OutputFile,OutputSequence)),nl,
	save_sequence_list_to_file(OutputFile,OutputSequence), % Save result to filename given
	write('model 1 terminated successfully.'),nl.

lost_learn([TrainingDataFile],_Options,ParametersFile) :-
	write('Training sample_model1'),nl,
	load_sequence_list_from_file(TrainingDataFile,InputSequence),
	prism(sample1),
	write(learn([sample1(InputSequence,_)])),
	check_or_fail(
		      learn([sample1(InputSequence,_)]), % e.g. unsupervised learning
		      error(prism_learning_failed)),
	write('sample_model1 was trained'),nl,
	check_or_fail(
		      save_sw(ParametersFile),
		      error(could_not_save_parameter_to_file(ParametersFile))),
	write('Pameters was saved to file: '),
	write(ParametersFile),nl.