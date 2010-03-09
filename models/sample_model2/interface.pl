:- ['../../lost.pl'].
:- lost_include_api(interface). 

% This is what is used to get the best annotation
lost_best_annotation([InputFile],Options,OutputFile) :-
	write('sample model 2: '),nl,
	write(lost_best_annotation(ParamFile,[InputFile],ExtraOptions,OutputFile)),nl,

	% Check and retrieve option values:
	lost_required_option(Options,parameter_file,ParamFile),
n
	% Get annotation for sample_model1 using parameters "test", the InputFile as
	% input and unify SampleModel1AnnotFile with the name of a file containing
	% the resulting annotation:
	lost_model_parameter_file(sample_model1, test, Model1ParamFile),
	get_annotation_file(sample_model1,
			    [InputFile],
			    [option(parameter_file,Model1ParamFile)],
			    SampleModel1AnnotFile),
	
	% Load the input sequences
	load_sequence_list_from_file(InputFile,InputSeq1),
	load_sequence_list_from_file(SampleModel1AnnotFile,InputSeq2),
	
	% Load the actual PRISM model
	prism(sample2),

	% Restore switch values:
	restore_sw(ParamFile),

	% Derive an annotation somehow:
	check_or_fail(
		      viterbig(sample2(InputSeq1,InputSeq2,OutputSequence)),
		      error(viterbig_says_no_no_no)),
	
	% Save result to filename given:
	save_sequence_list_to_file(OutputFile,OutputSequence),
	write('sample_model2 terminated successfully.'),nl.

