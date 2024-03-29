:- ['../../lost.pl'].
:- lost_include_api(interface).
:- lost_include_api(io).

lost_option(annotate,use_parameter_file,yes,'Load parameters from the parameter file').
	    
% Some ideas for specification of input & output formats...
lost_input_formats(annotate, [text(prolog(sequence)),text(prolog(prism_switches))]).
lost_input_formats(train,[text(prolog(sequence))]).
lost_output_format(annotate, _, text(prolog(sequence))).
lost_output_format(train, _, text(prolog(prism_swithches))).

% This is what is used to get the best annotation
annotate([InputFile,ParamFile],Options,OutputFile) :-
	write('sample model 1: '),nl,
	write(annotate([InputFile,ParamFile],Options,OutputFile)),nl,
	get_option(Options,use_parameter_file,UseParamFile),
	prism(sample1), % Load the actual PRISM model
	% If no parameter file was specificed, just use default (uniform) parameters:
	((UseParamFile == yes) ->
	 true ;
	 write(restore_sw(ParamFile)),nl,restore_sw(ParamFile)
	),
	get_data_from_file(InputFile,[data_position(4)],InputSequence),
        % Derive an annotation somehow
        write(viterbig(sample1(InputSequence,OutputSequence))),nl,
	check_or_fail(viterbig(sample1(InputSequence,OutputSequence)),
                        error(viterbig_says_no_no_no)),
        write('model 1 produced output sequence: '),
        write(OutputSequence),nl,
	save_annotation_to_sequence_file(sample_model1_annot,4,OutputSequence,OutputFile),
	write('model 1 terminated successfully.'),nl.

train([TrainingDataFile],_Options,ParametersFile) :-
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

