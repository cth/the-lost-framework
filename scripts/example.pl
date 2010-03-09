% This example illustrate how we use get_annotation to 
% have models run a files generated automatically

:- ['../lost.pl'].

% Prolog files in the shared directory can be consulted
% like this from anywhere..
:- lost_include_api(interface).

run_models_test :-
	% Unify InputSeqFile to the full filename of tinytest e.g. .../sequences/tinytest.seq
	lost_sequence_file(tinytest,InputSeqFile),

	% Parameter id "sample2" resolves to models/sample_model2/parameters/sample2.prb
	lost_model_parameter_file(sample_model2, sample2, ParameterFile),
	
	get_annotation_file(sample_model2,  % Name of model (resolves to models/sample_model2/)
			    [InputSeqFile], % A list of input files
			    [option(parameter_file,ParameterFile)],             % Extra options
			    AnnotFile),     % AnnotFile is unified to the name of the file that  annotation is written to

	% Load the sequence (AnnotSeq) contained in the file AnnotFile 
	load_sequence_list_from_file(AnnotFile,AnnotSeq),
	
	write('Resulting annotation sequence:'),nl,
	write(AnnotSeq),nl.

train_model_test :-
	lost_sequence_file(tinytest,InputSeqFile),
	train_model(sample_model1,
		    [InputSeqFile],
		    [], % No options
		    ModelParameterFile),
	write('Resulting parameter file:'),nl,
	write(ModelParameterFile),nl.
	

test_easygene_parser :-
        %lost_sequence_file('eg_U00096.dat'),
        get_annotation_file(parser_easygene,
                            _,
                            ['eg_U00096.dat'],
                            AnnotFile),
        write(AnnotFile).
