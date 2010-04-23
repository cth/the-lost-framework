:- ['../../lost.pl'].
:- lost_include_api(interface).
:- lost_include_api(io).

lost_option(lost_best_annotation,use_parameter_file,yes,'Load parameters from the parameter file').

lost_input_formats(lost_best_annotation, [text(prolog(sequence)),
					  text(prolog(sequence)),
					  text(prolog(prism_switches))]).

lost_output_format(lost_best_annotation, _, text(prolog(sequence))).


% This is what is used to get the best annotation
lost_best_annotation([InputFile1,InputFile2,ParamFile],Options,OutputFile) :-
	write('sample model 2: '),nl,
	write(lost_best_annotation([InputFile1,InputFile2,ParamFile],Options,OutputFile)),nl,

	% Check and retrieve option values:
	get_option(Options,use_parameter_file,UseParamFile),

	% Load the input sequences
	load_annotation_from_file(sequence,[data_position(4)],InputFile1,InputSeq1),
	load_annotation_from_file(sequence,[data_position(4)],InputFile2,InputSeq2),
	
	% Load the actual PRISM model
	prism(sample2),

	% If no parameter file was specificed, just use default (uniform) parameters:
	((UseParamFile == yes) -> (restore_sw(ParamFile)),nl,restore_sw(ParamFile) ; true),

	writeq(viterbig(sample2(InputSeq1,InputSeq2,OutputSequence))),nl,

	% Derive an annotation somehow:
	check_or_fail(
		      viterbig(sample2(InputSeq1,InputSeq2,OutputSequence)),
		      error(viterbig_says_no_no_no)),
	% Save result to filename given:
	save_annotation_to_sequence_file(sample_model2,4,OutputSequence,OutputFile),
	write('sample_model2 terminated successfully.'),nl.
