% This example illustrate how we use get_annotation to 
% have models run a files generated automatically

:- ['../lost.pl'].

% Prolog files in the shared directory can be consulted
% like this from anywhere..
:- lost_include_api(interface).

run_model1(ResultFile) :-
	lost_sequence_file(tinytest,TinySequence),
	lost_model_parameter_file(sample_model1, test, Model1ParamFile),
	get_annotation_file(sample_model1,
			    [TinySequence,Model1ParamFile],
			    [], 
			    ResultFile).

run_model2(ResultFile) :-
	run_model1(AnnotModel1),
	lost_sequence_file(tinytest,TinySequence),
	lost_model_parameter_file(sample_model2, sample2, ParameterFile),
	get_annotation_file(sample_model2,
			    [TinySequence,AnnotModel1,ParameterFile],
			    [use_parameter_file(no)],
			    ResultFile).

test_run :-
	run_model2(AnnotFile),
	write('Resulting annotation sequence file:'),nl,
	write(AnnotFile),nl,
	load_annotation_from_file(sequence,[data_position(4)],AnnotFile,AnnotSeq),
	write('Resulting annotation sequence:'),nl,
	write(AnnotSeq),nl.

test_train :-
	lost_sequence_file(tinytest,InputSeqFile),
	train_model(sample_model1,
		    [InputSeqFile],
		    [], % No options
		    ModelParameterFile),
	write('Resulting parameter file:'),nl,
	write(ModelParameterFile),nl.
