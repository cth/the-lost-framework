:- ['../../lost.pl'].
:- lost_include_api(interface). 

% This is what is used to get the best annotation
lost_best_annotation(ParamFile,[InputFile],ExtraOptions,OutputFile) :-
	write('sample model 2: '),nl,
	write(lost_best_annotation(ParamFile,[InputFile],ExtraOptions,OutputFile)),nl,	
	% Get annotation for sample_model1 using parameters "test", the InputFile as
	% input and unify SampleModel1AnnotFile with the name of a file containing
	% the resulting annotation:
	get_annotation_file(sample_model1,test,[InputFile],[],SampleModel1AnnotFile),
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
	write(save_sequence_list_to_file(OutputFile,OutputSequence)),
	save_sequence_list_to_file(OutputFile,OutputSequence),
	write('model 1 terminated successfully.'),nl.	

