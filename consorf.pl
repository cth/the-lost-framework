% This example illustrate how we use get_annotation to 
% have models run a files generated automatically

:- [lost].

% Prolog files in the shared directory can be consulted
% like this from anywhere..
:- lost_include_api(interface).

test_consorf :-
	% Unify InputSeqFile to the full filename of tinytest e.g. .../sequences/tinytest.seq
	lost_sequence_file('u00096-20k_cnk_+1_tx0_cns',InputConsFile),
	lost_sequence_file('u00096-20k_orf_+1',InputOrfFile), % ,ConsFile),

	% Parameter id "sample2" resolves to models/consorf_genefinder/parameters/consorf_genefinder.prb
	lost_model_parameter_file(consorf_genefinder, consorf_genefinder, ParameterFile),
	
	get_annotation_file(consorf_genefinder,  					% Name of model (resolves to models/sample_model2/)
			    [InputOrfFile,InputConsFile], 						% A list of input files
			    [option(parameter_file,ParameterFile)],   % Extra options
			    AnnotFile)     													% AnnotFile is unified to the name of the file that  annotation is written to

																										% Load the sequence (AnnotSeq) contained in the file AnnotFile 
	/*
	load_annotation_from_file(AnnotFile,AnnotSeq),
	writeln('here i am'),
	
	write('Resulting annotation sequence:'),nl,
	write(AnnotSeq),nl
	*/.


